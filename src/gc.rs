use crate::value::{HeapObj, HeapObjVal, Value};

use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashMap;

const DEBUG_GC: bool = false;
const DEBUG_STRESS_GC: bool = false;

const INIT_GC_THRESHOLD: usize = 200;
const MIN_SCALING_FACTOR: f64 = 0.5;
const MAX_SCALING_FACTOR: f64 = 2.0;
const SHRINK_THRESHOLD: f64 = 0.75; // Shrink if new_size < current_size * shrink_threshold => close to 1 means lots of shrinks, close to 0 means rarely shrink

// The garbage collector. Let's go

// Important note to make sure I never break the GC:
// The only way we can deallocate something that we didnt mean to is if we have a LoxPointer somewhere other than the stack, globals, or in a reachable HeapObj
// So be careful if we ever alloc a LoxClosure or a LoxInstance when a LoxPointer is floating around on the rust stack
//   => ie popping a value off, calling alloc, and then doing something with that value. If that value is the last pointer to an instance, it'll cause the instance to be deleted

// Ideas for making this not have placeholder values / fewer placeholder values:
// Steps to making it work:
// 1. Use a linked list for instances
// 2. When removing a value, replace the node with a placeholder
// 3. When removing a value next to a placeholder, merge the two together and increment a counter in the placeholder
// 4. When traverseing the linked list, a placeholder is worth {counter} slots
// 5. When allocating values, use the same queue but if it hits the middle of a placeholder, split it down the middle into two placeholders?
//   => Ideally we would not but since the free_slots stack is not ordered in any way we don't have any guarentees

// This would get rid of most of the placeholder values but with a few problems
// A. The memory usage of the placeholders is minimal already
// B. Placeholders don't necessarily "leak", ie: running the program for a long enough time will not cause a memory shortage unless the code itself had used that much memory without GCing
// C. Linked lists and doing those traversals will undoubltly be slower than the current Vec implementation, will it even be worth it?

// All in all, I think I'll need to wait until I have some code to profile. (but since this is a for fun compiler this is just short for "im never going to do this unless i have some spare time and have nothing better to do")

pub struct GC {
    pub instances: Vec<Box<HeapObj>>,

    allocations: usize,       // The number of live allocations
    next_gc_threshold: usize, // The number of allocations allowed until we GC

    grey_worklist: Vec<usize>, // Each worklist task is an index into the instances vec for the HeapObj
    free_slots: BinaryHeap<Reverse<usize>>, // A priority queue for which slots to allocate. A min-heap because we want to allocate the front slots of the instances vec first,
                                            // so that the later slots (which are still filled but just with placeholders) can be truncated in the cases where a users program allocates a large amount, drops them all, and then leavesthe instances vec full of placeholders

                                            // unmarked: bool, // Which bool type represents an "unmarked" node
                                            // Annoying to implement because new variables will get instantiated with the wrong value, possibly allowing them to live one extra round of GC
}

impl GC {
    pub fn alloc(
        &mut self,
        val: HeapObj,
        stack: &Vec<Value>,
        globals: &HashMap<String, Value>,
    ) -> Value {
        if DEBUG_STRESS_GC || self.allocations >= self.next_gc_threshold {
            self.collect_garbage(stack, globals);
        }

        self.instances.push(Box::new(val)); // Either way we need to put on the new instance
        let index = if self.free_slots.is_empty() {
            self.instances.len() - 1
        } else {
            match self.free_slots.pop() {
                Some(Reverse(index)) => {
                    // Swap the instance over to it's slot and remove the placeholder that used to be there
                    let placeholder = self.instances.swap_remove(index);
                    if placeholder.obj != HeapObjVal::HeapPlaceholder {
                        panic!("VM error! GC attempted to use an allocated value as a free slot")
                    }
                    index
                }
                None => panic!("VM panic!"),
            }
        };

        self.allocations += 1;
        if DEBUG_GC {
            eprintln!(
                "allocated slot {} | # of allocations = {}",
                index, self.allocations
            )
        }
        Value::LoxPointer(index)
    }

    /// Doesn't quite "reclaim" memory, as rather just creating an empty slot where new allocations can go
    /// I don't think I can properly reclaim the memory unless I do some unsafe uninitialized memory stuff
    fn free(&mut self, index: usize) {
        if DEBUG_GC {
            eprintln!(
                "freed slot {} | # of allocations = {} | value to free = {:?}",
                index,
                self.allocations,
                self.instances.get(index)
            )
        }

        self.instances.push(Box::new(HeapObj::new_placeholder()));
        self.instances.swap_remove(index);
        self.free_slots.push(Reverse(index));
        self.allocations -= 1;
    }

    fn mark_heap_obj(&mut self, index: usize) {
        let obj_opt = self.instances.get_mut(index);
        match obj_opt {
            Some(obj) => {
                if obj.is_marked == false {
                    // obj.is_marked = !self.unmarked;
                    obj.is_marked = true;
                    if DEBUG_GC {
                        eprintln!("marked {:?} at {}", obj.obj_type, index)
                    }
                    self.grey_worklist.push(index); // Only the values that are pointed to by LoxPointers (instances and closures) can contain values that might be garbage collected
                }
            }
            None => panic!("VM panic! Why is there an unallocated pointer?"),
        }
    }

    fn mark_value(&mut self, val: &Value) {
        if let Value::LoxPointer(ptr) = val {
            self.mark_heap_obj(*ptr);
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
                    if DEBUG_GC {
                        eprintln!("blackening {:?} at {}", obj.obj_type, index)
                    }
                    match &obj.obj {
                        HeapObjVal::LoxClosure(closure) => {
                            // Fixme: This really really isn't great for performance
                            // The borrow checker gets upset because closure is immutable but can be modified by the &mut self call to mark_value
                            for val in closure.values.clone() {
                                self.mark_value(&val);
                            }
                        },
                        HeapObjVal::LoxInstance(instance) => {
                            // Fixme: Same issue as LoxClosure
                            for val in instance.fields.clone().values() {
                                self.mark_value(&val);
                            }
                        },
                        HeapObjVal::HeapPlaceholder => { panic!("VM panic! Why do we have a valid reference to a heap placeholder value?")},

                    }
                }
                None => panic!("VM panic! Why is there an unallocated pointer?"),
            }
        }
    }

    fn sweep(&mut self) -> Option<usize> {
        let mut to_free = Vec::new();
        let mut shrinkable_to: Option<usize> = None;

        for (i, obj) in self.instances.iter_mut().enumerate() {
            if obj.obj == HeapObjVal::HeapPlaceholder {
                match shrinkable_to {
                    Some(_) => {}
                    None => shrinkable_to = Some(i),
                }
            } else {
                shrinkable_to = None;
                if !obj.is_marked {
                    to_free.push(i);
                } else {
                    obj.is_marked = false;
                }
            }
        }

        // Free the unmarked instances
        for index in to_free.iter() {
            self.free(*index);
        }

        shrinkable_to
    }

    /// Shrink the instances vec as much as possible, but only if we will be removing above a given threshold # of placeholders
    fn shrink(&mut self, new_size: usize) {
        if (new_size as f64) < SHRINK_THRESHOLD * (self.instances.len() as f64) {
            if DEBUG_GC {
                eprintln!("shrinking from {} to {}", self.instances.len(), new_size)
            }
            self.instances.truncate(new_size);

            // Make sure that we remove those indices from free_slots
            let mut new_slots: BinaryHeap<Reverse<usize>> = BinaryHeap::new();
            for x in self.free_slots.drain() {
                match x {
                    Reverse(i) => {
                        if i < new_size {
                            new_slots.push(x)
                        }
                    }
                }
            }
            self.free_slots = new_slots;
        } else {
            if DEBUG_GC {
                eprintln!(
                    "not shrinking from {} to {}",
                    self.instances.len(),
                    new_size
                )
            }
        }
    }

    /// Rescale the GC threshold. Called after all garbage collections
    fn rescale_threshold(&mut self) {
        // Now we know we went from self.next_gc_threshold # of instances down to self.allocations # of instances
        // Use that difference to determine the next_gc_threshold
        let diff = self.next_gc_threshold - self.allocations;

        // 0 <= diff <= old_threshold
        // If this number is small, then we have mostly live values, and we should let the heap grow quite a bit before we try to GC again
        // => If diff = 0, double the threshold

        // If this numbere is large, then we should GC more often
        // => if diff = old_threshold, halve the threshold

        let old_threshold = self.next_gc_threshold as f64;
        let slope: f64 = (MIN_SCALING_FACTOR - MAX_SCALING_FACTOR) / old_threshold;
        let scaling_factor = slope * (diff as f64) + MAX_SCALING_FACTOR;
        let new_threshold = old_threshold * scaling_factor;
        self.next_gc_threshold = 1 + new_threshold as usize;

        if DEBUG_GC {
            eprintln!(
                "Scaled GC threshold from {} to {}",
                old_threshold, self.next_gc_threshold
            );
        }
    }

    fn collect_garbage(&mut self, stack: &Vec<Value>, globals: &HashMap<String, Value>) {
        if DEBUG_GC {
            eprintln!("--- gc begin")
        }

        self.mark_roots(stack, globals);
        self.mark_grey();
        let shrinkable_to = self.sweep();

        if let Some(new_size) = shrinkable_to {
            self.shrink(new_size);
        }

        if DEBUG_GC {
            // # of collections this round is inaccurate if we have DEBUG_GC_STRESS turned on, since we don't use the threshold
            eprintln!(
                "After collection | vec_size = {} | allocations = {} | collected = {}",
                self.instances.len(),
                self.allocations,
                self.next_gc_threshold - self.allocations
            );
        }

        self.rescale_threshold();

        //self.unmarked = !self.unmarked; // Flip for the next gc run
        if DEBUG_GC {
            eprintln!("--- gc end")
        }
    }

    pub fn new() -> GC {
        GC {
            grey_worklist: Vec::new(),
            instances: Vec::new(),
            free_slots: BinaryHeap::new(),
            allocations: 0,
            next_gc_threshold: INIT_GC_THRESHOLD,
        }
    }
}
