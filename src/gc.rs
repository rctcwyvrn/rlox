use crate::value::{Value, HeapObj, HeapObjVal, ObjPointer};

use std::collections::HashMap;

const DEBUG_GC: bool = true;
const DEBUG_STRESS_GC: bool = true;

const INIT_GC_THRESHOLD: usize = 100;
const MIN_SCALING_FACTOR: f64 = 0.5;
const MAX_SCALING_FACTOR: f64 = 1.2;

// The garbage collector. Lets go
// The only way we can deallocate something that we didnt mean to is if we have a LoxPointer somewhere other than the stack, globals, or in a reachable HeapObj 
// So be careful if we ever alloc a LoxClosure or a LoxInstance when a LoxPointer is floating around on the rust stack
// => ie popping a value off, calling alloc, and then doing something with that value

pub struct GC {
    pub instances: Vec<Box<HeapObj>>,

    allocations: usize, // The number of live allocations
    next_gc_threshold: usize, // The number of allocations allowed until we GC

    grey_worklist: Vec<usize>, // Each worklist task is an index into the instances vec for the HeapObj
    free_slots: Vec<usize>,
    // unmarked: bool, // Which bool type represents an "unmarked" node
                       // Annoying to implement because new variables will get instantiated with the wrong value, possibly allowing them to live one extra round of GC
}

impl GC {
    pub fn alloc(&mut self, val: HeapObj, stack: &Vec<Value>, globals: &HashMap<String, Value>) -> Value {
        if DEBUG_STRESS_GC || self.allocations >= self.next_gc_threshold {
            self.collect_garbage(stack, globals);
        }

        let index = if self.free_slots.is_empty() {
            self.instances.push(Box::new(val));
            self.instances.len() - 1
        } else {
            let index = self.free_slots.pop().unwrap();
            self.instances.push(Box::new(val));
            self.instances.swap_remove(index);
            index
        };

        self.allocations+=1;
        if DEBUG_GC { eprintln!("allocated slot {} | # of allocations = {}", index, self.allocations) }
        Value::LoxPointer(ObjPointer { obj: index })
    }

    /// Doesn't quite "reclaim" memory, as rather just creating an empty slot where new allocations can go
    /// I don't think I can properly reclaim the memory unless I do some unsafe uninitialized memory stuff
    fn free(&mut self, index: usize) {
        if DEBUG_GC { eprintln!("freed slot {} | # of allocations = {} | value to free = {:?}", index, self.allocations, self.instances.get(index)) }

        self.instances.push(Box::new(HeapObj::new_placeholder()));
        self.instances.swap_remove(index);
        self.free_slots.push(index);
        self.allocations-=1;
    }

    fn mark_heap_obj(&mut self, index: usize) {
        let obj_opt = self.instances.get_mut(index);
        match obj_opt {
            Some(obj) => {
                if obj.is_marked == false {
                    // obj.is_marked = !self.unmarked;
                    obj.is_marked = true;
                    if DEBUG_GC { eprintln!("marked {:?} at {}", obj.obj_type, index) }
                    self.grey_worklist.push(index); // Only the values that are pointed to by LoxPointers (instances and closures) can contain values that might be garbage collected
                }
            },
            None => panic!("VM panic! Why is there an unallocated pointer?"),
        }
    }

    fn mark_value(&mut self, val: &Value) {
        if let Value::LoxPointer(ptr) = val {
            self.mark_heap_obj(ptr.obj);
        }
    }

    fn mark_roots(&mut self, stack: &Vec<Value>, globals: &HashMap<String, Value>) {
        for val in stack.iter() {
            self.mark_value(val);
        }

        for val in globals.values() {
            self.mark_value(val);
        }
    }

    fn mark_grey(&mut self) {
        while !self.grey_worklist.is_empty() { 
            let index = self.grey_worklist.pop().unwrap();
            let obj_opt = self.instances.get(index);
            match obj_opt {
                Some(obj) => { 
                    // Blacken -> Look for LoxPointers that might be stored in these HeapObjs
                    if DEBUG_GC { eprintln!("blackening {:?} at {}", obj.obj_type, index) }
                    match &obj.obj {
                        HeapObjVal::LoxClosure(closure) => {
                            // This really really isn't great for performance
                            // The borrow checker gets upset because closure is immutable but can be modified by the &mut self call to mark_value
                            for val in closure.values.clone() {
                                self.mark_value(&val);
                            }
                        },
                        HeapObjVal::LoxInstance(instance) => {
                            // Same issue as LoxClosure
                            for val in instance.fields.clone().values() {
                                self.mark_value(&val);
                            }
                        },
                        HeapObjVal::HeapPlaceholder => { panic!("VM panic! Why do we have a valid reference to a heap placeholder value?")},

                    }
                },
                None => panic!("VM panic! Why is there an unallocated pointer?"),
            }
        }
    }

    fn sweep(&mut self) {
        let mut to_free = Vec::new();
        for (i, obj) in self.instances.iter_mut().enumerate() {
            if !(obj.obj == HeapObjVal::HeapPlaceholder) &&  !obj.is_marked {
                to_free.push(i);
            } else {
                obj.is_marked = false;
            }
        }

        for index in to_free.iter() {
            self.free(*index);
        }
    }

    /// Rescale the GC threshold. Called after all garbage collections
    /// 
    /// Note: I'm doing this because I don't know how to actually do the unsafe rust thing and have unallocated memory
    fn rescale_threshold(&mut self) {
        // Now we know we went from self.next_gc_threshold # of instances down to self.allocations # of instances
        // Use that difference to determine the next_gc_threshold
        let diff = self.next_gc_threshold - self.allocations;

        // If this number is small, then we have mostly live values, and we should let the heap grow quite a bit before we try to GC again
        // => If diff = 0, double the threshold

        // If this numbere is large, then we should GC more often
        // => if diff = old_threshold, halve the threshold

        let old_threshold = self.next_gc_threshold as f64;
        let slope = (MAX_SCALING_FACTOR - MIN_SCALING_FACTOR)/ old_threshold;
        let scaling_factor = slope * (diff as f64);
        let new_threshold = old_threshold * scaling_factor;
        self.next_gc_threshold = 1 + new_threshold as usize;

        if DEBUG_GC {
            eprintln!("Scaled GC threshold from {} to {}", old_threshold, self.next_gc_threshold);
        }
    }

    fn collect_garbage(&mut self, stack: &Vec<Value>, globals: &HashMap<String, Value>) {
        if DEBUG_GC { eprintln!("--- gc begin") }

        self.mark_roots(stack, globals);
        self.mark_grey();
        self.sweep();

        if DEBUG_GC {
            eprintln!("After collection | vec_size = {} | allocations = {} | collected = {}", self.instances.len(), self.allocations, self.next_gc_threshold - self.allocations);
        }

        self.rescale_threshold();

        //self.unmarked = !self.unmarked; // Flip for the next gc run
        if DEBUG_GC { eprintln!("--- gc end") }

    }

    pub fn new() -> GC {
        GC {
            grey_worklist: Vec::new(),
            instances: Vec::new(),
            free_slots: Vec::new(),
            allocations: 0,
            next_gc_threshold: INIT_GC_THRESHOLD,
        }
    }
}
