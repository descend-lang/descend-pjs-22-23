import sys



with open("output.log", "r") as f:
    with open(f"{sys.argv[1]}.csv", "wt") as fout:
        data = f.read()
        out = ""
        for line in data.split("\n"):
            if(line.startswith("time<<<")):
                out = out + line.split("time<<<")[1] + "\n"

        data = "kernel\n" + out
        fout.write(data)