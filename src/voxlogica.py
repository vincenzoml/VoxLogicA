
import getpass
import tempfile
import subprocess
import json
import re
import os
import pandas as pd
from pathlib import Path
import graphviz

voxlogica_bin = "./VoxLogicA/binaries/VoxLogicA_1.0-experimental_linux-x64/VoxLogicA"


def save(str, filename):
    with open(filename, "w") as f:
        f.write(str)


def parse_times(output):
    def time(line):
        return line[1:11]

    def msg(line):
        return line[22:]
    lines = [(time(line), msg(line)) for line in output.splitlines()]

    def time_by_re(rex):
        try:
            return int([line[0] for line in lines if re.search(rex, line[1])][0])
        except:
            return None
    start = int(lines[0][0])
    computation = time_by_re("^Starting computation...$")
    gpu = time_by_re("^GPU initialized$")
    total = time_by_re("^... done.$")
    performance = time_by_re("^Start measuring performance from here$")
    delta = total - (performance if performance else max(gpu,
                     computation) if gpu else computation)
    return {"tBegin": start, "tStartCompute": computation, "tGPU": gpu, "tMeasurePerformance": performance, "tEnd": total, "delta": delta}


def run_voxlogica(contents=None, filename=None, substitutions={}, gpu=True, ptest=False, debug=False, taskGraph=False):
    new_env = dict(os.environ)
    new_env['LC_ALL'] = 'C'
    bin = voxlogica_bin
    if (not (filename or contents)):
        raise Exception("Please provide either filename or contents")
    else:
        if contents:
            spec = contents
        else:
            f = open(filename, "r")
            spec = f.read()
            f.close()
        for sub in substitutions:
            spec = spec.replace("$"+sub, str(substitutions[sub]))
        fname = tempfile.NamedTemporaryFile().name
        f = open(fname, "w")
        f.write(spec)
        f.close()
        tgfname = ""
        try:
            args = [bin]
            if not debug:
                args.append("--json")
            if ptest:
                args.append("--performancetest")
            if taskGraph:
                tgfname = tempfile.NamedTemporaryFile().name
                args.append("--savetaskgraph")
                args.append(tgfname)
            args.append(fname)
            output = subprocess.check_output(
                args, env=new_env, stderr=subprocess.STDOUT, universal_newlines=True)
            exitcode = 0
        except subprocess.CalledProcessError as exc:
            output = exc.output
            exitcode = exc.returncode
        if taskGraph:
            # file = open(tgfname,mode='r')
            # dotf = file.read()
            # file.close()
            dot = graphviz.Source.from_file(tgfname)
            return dot
        else:
            if debug:
                return output
            else:
                res = json.loads(output)
                res["exitcode"] = exitcode
                if exitcode == 0:
                    res["raw_output"] = output
                    res["values"] = {record["name"]: (float(
                        record["value"]) if record["vltype"] == "number" else record["value"]) for record in res["print"]}
                    res["times"] = parse_times(res["log"])
                else:
                    print(res["log"])
                return res


def simplify_results(voxlogica_output):
    converter = {
        "bool": lambda v: (v.lower() in ("yes", "true", "t", "1")),
        "number": float
    }
    return [
        {
            "filename": output["filename"],
            "results": {
                res['name']: converter[res['vltype']](res['value'])
                for res in output['output']['print']
            } | {
                layer['name']: {"filename": layer['name'] + layer['extension']}
                for layer in output['output']['layers']
            }
        }
        for output in voxlogica_output
    ]
