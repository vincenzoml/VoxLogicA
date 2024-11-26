#!/usr/bin/env python3

import argparse

def main():
    parser = argparse.ArgumentParser(description='Process a file.')
    parser.add_argument('filename', type=str, help='The name of the file to process')
    
    args = parser.parse_args()
    
    try:
        with open(args.filename, 'r') as file:
            import subprocess
            name = args.filename.rsplit('.', 1)[0]
            outputName = f"{name}-temporal.imgql"
            dotName = f"{name}-temporal.dot"
            outputNameNone = f"{name}-temporal-new.imgql"
            dotNameNone = f"{name}-temporal-new.dot"

            subprocess.run(["dotnet", "run", "--", args.filename,"--savetaskgraphasprogram", outputName, "--providecontext", "n"],stderr=subprocess.STDOUT)
            subprocess.run(["dotnet", "run", "--", outputName,"--savetaskgraphasdot", dotName],stderr=subprocess.STDOUT) 
            subprocess.run(["dotnet", "run", "--", outputName,"--savetaskgraphasprogram", outputNameNone],stderr=subprocess.STDOUT)
            subprocess.run(["dotnet", "run", "--", outputNameNone,"--savetaskgraphasdot", dotNameNone],stderr=subprocess.STDOUT)           
       
    except FileNotFoundError:
        
        print("File not found")

if __name__ == "__main__":
    main()
