#!/usr/bin/env python3

import argparse

def main():
    parser = argparse.ArgumentParser(description='Process a file.')
    parser.add_argument('filename', type=str, help='The name of the file to process')
    parser.add_argument('numFrames', type=int, help='The number of frames to process')
    
    args = parser.parse_args()
    
    try:
        with open(args.filename, 'r') as file:
            import subprocess
            name = args.filename.rsplit('.', 1)[0]
            numFrames = str(args.numFrames)
            outputName = f"{name}-temporal.imgql"
            dotName = f"{name}-temporal.dot"
            outputNameNone = f"{name}-temporal-new.imgql"
            dotNameNone = f"{name}-temporal-new.dot"
            outputNameVL1 = f"{name}-temporal-new-VL1.imgql"
            dotNameVL1 = f"{name}-temporal-newVL1.dot"

            subprocess.run(["bin/release/net8.0/linux-x64/VoxLogicA", args.filename, "--numframes", numFrames,"--savetaskgraphasprogram", outputName, "--providecontext", "n"],stderr=subprocess.STDOUT)
            subprocess.run(["bin/release/net8.0/linux-x64/VoxLogicA", outputName,"--savetaskgraphasdot", dotName],stderr=subprocess.STDOUT) 
            subprocess.run(["bin/release/net8.0/linux-x64/VoxLogicA", outputName, "--numframes", numFrames,"--savetaskgraphasprogram", outputNameNone],stderr=subprocess.STDOUT)
            subprocess.run(["bin/release/net8.0/linux-x64/VoxLogicA", outputNameNone,"--savetaskgraphasdot", dotNameNone],stderr=subprocess.STDOUT)           
            subprocess.run(["bin/release/net8.0/linux-x64/VoxLogicA", outputNameNone, "--numframes", numFrames,"--evaluatespatiotemporal", outputNameVL1],stderr=subprocess.STDOUT)        
       
    except FileNotFoundError:
        
        print("File not found")

if __name__ == "__main__":
    main()
