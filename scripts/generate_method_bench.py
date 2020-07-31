with open("./method_bench.lox", "w") as f:
    f.write("class Test { test() {}}\n")
    f.write("fun batch() {\n")
    f.write(f"\tvar test = Test();\n")
    for i in range(10000):
        f.write(f"\ttest.test();\n")
    f.write("}\n")
    for i in range(1000):
        f.write("batch();\n")

    # 2.134s unoptimized
    # 3.531s for jlox
    # 3.763 with single param opcodes only => not where the new bottleneck is
    # 3.678 with manual inlineing of invoke_from_class
    # 1.667 with doing everything in VM instead of VM state
        # My hypothesis is that it mainly has to do with following the references to get to the function and class defs
        # The improvement is nowhere near the one in clox because we had very small ObjBoundMethods that were stack allocated instead of heap