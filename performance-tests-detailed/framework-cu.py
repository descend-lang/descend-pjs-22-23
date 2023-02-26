import sys



with open("output.log", "r") as f:
    with open(f"{sys.argv[1]}.csv", "wt") as fout:
        data = f.read()
        data = data.replace("kernel:reduce, Total kernel runtime","")
        # data = data.replace("kernel:reduce,Total kernel runtime","")
        # data = data.replace("kernel:inplace_vector_add,Total kernel runtime","")
        data = data.replace(", ",",")
        data = data.replace(",\n\n","+")
        data = data.replace("+","\n")
        data = data[1:-2]
        # data = "kernel_cu,kernel_cu\n" + data
        fout.write(data)