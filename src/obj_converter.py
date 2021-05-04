## obj to imgq file
import sys
import os


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
        colorCode = lineData[4]+lineData[5]+lineData[6]
        if colorCode not in properties:
            properties[colorCode] = idCounter
            idCounter += 1
        currentProp = "p" + str(properties[colorCode])
        points.append( [lineData[1], lineData[2], lineData[3], currentProp] )

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
fout.write( '"'+ '",\n"'.join( [ 'p'+str(i) for i in range(len(properties)) ] ) + '"' )

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
    '["'+point[3]+'"]'
    ) for i,point in enumerate(points) ] ) + ',' )
# # edges
edgeCounter = 0
edgeSimplexesStrings = []
for e1 in edges:
    for e2 in edges[e1]:
        # print(e2)
        edgeSimplexesStrings.append(
            simplexString(
                'E'+str(edgeCounter),
                '['+str(e1)+','+str(e2)+']',
                '["'
                +points[e1][3]+'","'
                +points[e2][3]+'"]'
            )
        )
        edgeCounter += 1
fout.write( ','.join(edgeSimplexesStrings) + ',' )
# triangles
triaCounter = 0
triaSimplexesStrings = []
for tria in triangles:
    triaSimplexesStrings.append(
        simplexString(
            'T'+str(triaCounter),
            '['+str(tria[0])+','+str(tria[1])+','+str(tria[2])+']',
            '["'+points[tria[0]][3]+'","'+points[tria[1]][3]+'","'+points[tria[2]][3]+'"]'
        )
    )
    triaCounter += 1
fout.write( ','.join(triaSimplexesStrings) )

fout.write(']}')
fout.close()


