## obj to imgq file
import sys
import os

def quantize(c):
    return int(c >= 0.3)

with open(sys.argv[1]) as fin:
    objContent = fin.readlines()
objContent = [x.strip() for x in objContent]

# collect the points
points = []
properties = {}
idCounter = 0
for line in objContent:
    # print(line)
    lineData = line.split()
    # print(lineData)
    if lineData and (lineData[0] == "v"):
        r = float(lineData[4]) #round(float(lineData[4]) * 255)
        g = float(lineData[5]) #round(float(lineData[5]) * 255)
        b = float(lineData[6]) #round(float(lineData[6]) * 255)
        colorCode = [r,g,b]
        label = str(colorCode)
        if label not in properties:
            properties[label] = colorCode                    
        points.append( [lineData[1], lineData[2], lineData[3], colorCode] )

# collect the edges and triangles
edges = {}
triangles = []
for line in objContent:
    lineData = line.split()
    if lineData and (lineData[0] == "f"):
        # print(line)
        pcode = sorted([ int(lineData[i+1].split("//")[0])-1 for i in range(3) ])
        # print(pcode)
        for (i,j) in [(0,1),(1,2),(0,2)]:
            m = pcode[i]
            M = pcode[j]
            if m in edges:
                edges[m].append(M)
            else:
                edges[m] = [M]
        triangles.append(pcode)

# create and fill json file
foutName = os.path.splitext(sys.argv[1])[0]+'.json'
fout = open(foutName, "w")
fout.write('{')

fout.write('"numberOfPoints": ')
fout.write(str(len(points)))

fout.write(',"coordinatesOfPoints": [')
fout.write( ',\n'.join( [ '['+','.join([str(c) for c in point[:3]])+']' for point in points ] ) )

fout.write('],"atomNames": [')
# fout.write( '"'+ '",\n"'.join( [ 'p'+str(i) for i in range(len(properties)) ] ) + '"' )
fout.write( '"r0","r1","g0","g1","b0","b1"')

def simplexString(id,points,atoms):
    result = '{"id":"'
    result += id
    result += '","points":'
    result += points
    result += ',"atoms":'
    result += atoms
    result += '}\n'
    return result


fout.write('],"simplexes": [')
# points
fout.write( ','.join(  [ simplexString(
    'P'+str(i),
    '['+str(i)+']',
    '["'+'r'+str(quantize(point[3][0]))+'","'+'g'+str(quantize(point[3][1]))+'","'+'b'+str(quantize(point[3][2]))+'"]'
    ) for i,point in enumerate(points) ] ) + ',' )
# # edges
edgeCounter = 0
edgeSimplexesStrings = []
for e1 in edges:
    for e2 in edges[e1]:
        # print(e2)
        color = [(points[e1][3][0] + points[e2][3][0]) / 2,(points[e1][3][1] + points[e2][3][1]) / 2,(points[e1][3][2] + points[e2][3][2]) / 2]        
        edgeSimplexesStrings.append(
            simplexString(
                'E'+str(edgeCounter),
                '['+str(e1)+','+str(e2)+']',                
                #+ str(color) + 
                '["'+'r'+str(quantize(color[0]))+'","'+'g'+str(quantize(color[1]))+'","'+'b'+str(quantize(color[2]))+'"]'                
            )
        )
        edgeCounter += 1
fout.write( ','.join(edgeSimplexesStrings) + ',' )
# triangles
triaCounter = 0
triaSimplexesStrings = []
for tria in triangles:
    color = [
        (points[tria[0]][3][0] + points[tria[1]][3][0] + points[tria[2]][3][0]) / 3,
        (points[tria[0]][3][1] + points[tria[1]][3][1] + points[tria[2]][3][1]) / 3,
        (points[tria[0]][3][2] + points[tria[1]][3][2] + points[tria[2]][3][2]) / 3
    ]
    triaSimplexesStrings.append(
        simplexString(
            'T'+str(triaCounter),
            '['+str(tria[0])+','+str(tria[1])+','+str(tria[2])+']',
            '["' + 'r'+str(quantize(color[0]))+'","'+'g'+str(quantize(color[1]))+'","'+'b'+str(quantize(color[2]))+'"]' 
        )
    )
    triaCounter += 1
fout.write( ','.join(triaSimplexesStrings) )

fout.write(']}')
fout.close()


