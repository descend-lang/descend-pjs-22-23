import sys



with open("output.log", "r") as f:
    with open(f"{sys.argv[1]}.csv", "wt") as fout:
        data = f.read()
        data = data.replace("real","")
        data = data.replace("user","")
        data = data.replace("sys","")
        data = data.replace("\t","")
        data = data.replace("\n\n","+")
        data = data.replace("\n",",")
        data = data.replace("+","\n")
        data = data.replace("0m","")
        data = data.replace("s","")
        data = data[1:-1]
        data = "real,user,sys\n" + data
        fout.write(data)