import sys



with open("output.log", "r") as f:
    with open(f"{sys.argv[1]}.csv", "wt") as fout:
        data = f.read()
        out = ""
        for line in data.split("\n"):
            if(line.startswith("time<<<")):
                time_unit = line.split("time<<<")[1]
                time_unit = float(time_unit)
                time_unit = time_unit / 1000000
                out = f"{out}{time_unit}\n"

        data = "kernel\n" + out
        fout.write(data)