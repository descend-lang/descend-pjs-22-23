import sys



with open("output.log", "r") as f:
    with open(f"{sys.argv[1]}.csv", "wt") as fout:
        data = f.read()
        data = data.replace("kernel:reduce_shared_mem, Total kernel runtime","")
        data = data.replace(", ",",")
        data = data.replace(",\n\n","+")
        data = data.replace("+","\n")
        data = data[1:-2]
        data = "kernel,kernel\n" + data
        fout.write(data)