with open("./gc_stress_extreme.lox", "w") as f:
    f.write("class Test {}\n")
    for i in range(70000):
        f.write(f"var abc{i} = {i};\n")
    f.write("for(var x = 0; x<=10; x = x + 1) {\n")
    for i in range(70000):
        f.write(f"\tabc{i} = Test();\n")
        f.write(f"\tabc{i}.val = \"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\";\n")
    f.write("}\n")