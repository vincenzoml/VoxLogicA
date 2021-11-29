import json




# load maze file
with open('./maze.json') as f:
  mazeData = json.load(f)


nodesData = mazeData["nodes"]
gridSizes = {
  "x": max( [ x["coord"]["x"]  for x in nodesData ] ) + 1,
  "y": max( [ y["coord"]["y"]  for y in nodesData ] ) + 1,
  "z": max( [ z["coord"]["z"]  for z in nodesData ] ) + 1
}
# print(gridSizes)

# auxiliary function to encode the coordinates
def encodeCoords( coords ):
  return coords["x"] * gridSizes["y"] * gridSizes["z"] + coords["y"] * gridSizes["z"] + coords["z"]


linksData = mazeData["links"]
numberOfLinks = len(linksData) + 1


# some constants
POINTS_IN_THE_GRID      = gridSizes["x"] * gridSizes["y"] * gridSizes["z"] 
POINTS_IN_A_ROOM        = 6
EDGES_IN_A_ROOM         = 13
TRIANGLES_IN_A_ROOM     = 12
TETHRAEDRONS_IN_A_ROOM  = 4
SIMPLEXES_IN_A_ROOM     = POINTS_IN_A_ROOM + EDGES_IN_A_ROOM + TRIANGLES_IN_A_ROOM + TETHRAEDRONS_IN_A_ROOM
# POINTS_IN_A_CORRIDOR = 0

TRANSLATE_DISTANCE_ROOMS  = 30

NUMBER_OF_ROOMS = POINTS_IN_THE_GRID
NUMBER_OF_CORRIDORS = numberOfLinks
SIMPLEXES_IN_A_CORRIDOR = 1

INDEX_ROOM_SIMPLEXES = 0
INDEX_CORRIDOR_SIMPLEXES = NUMBER_OF_ROOMS * SIMPLEXES_IN_A_ROOM





# generate grid of points of a default room
roomPointCoords = [ [-5,0,0], [5,0,0], [0,-5,0], [0,5,0], [0,0,-5], [0,0,5] ]

# generate the simplexes of a default room
roomZeroSimplexes   = [ [0], [1], [2], [3], [4], [5] ]
roomOneSimplexes    = [ [0,1], [0,2], [0,3], [0,4], [0,5], [1,2], [1,3], [1,4], [1,5], [2,4], [2,5], [3,4], [3,5] ]
roomTwoSimplexes    = [ [0,1,2], [0,1,3], [0,1,4], [0,1,5], [0,2,4], [0,2,5], [0,3,4], [0,3,5], [1,2,4], [1,2,5], [1,3,4], [1,3,5] ]
roomThreeSimplexes  = [ [0,1,2,4], [0,1,2,5], [0,1,3,4], [0,1,3,5] ]


# auxiliary function to translate the coordinates
def translateCoordinates( listOfCoordinates, shiftX, shiftY, shiftZ ):
  result = [  [ c[0] + shiftX, c[1] + shiftY, c[2] + shiftZ ] for c in listOfCoordinates  ]
  return result

# auxiliary function to translate the simplex indices
def translateSimplexIndices( listOfSimplexIndices, shift ):
  result = [  [ x + shift for x in s ]  for s in listOfSimplexIndices  ]
  return result



# Fill the coordinates of points
coordinatesOfPoints = []
for x in range(gridSizes["x"]):
  for y in range(gridSizes["y"]):
    for z in range(gridSizes["z"]):
      resTranslation = translateCoordinates( roomPointCoords, x * TRANSLATE_DISTANCE_ROOMS, y * TRANSLATE_DISTANCE_ROOMS, z * TRANSLATE_DISTANCE_ROOMS )
      coordinatesOfPoints.extend(resTranslation)


# Fill the list of simplexes with the room and corridor data
listOfSimplexes = []
# add simplexes of rooms
for roomIndex in range( NUMBER_OF_ROOMS ):
  listOfSimplexes.extend( translateSimplexIndices( roomZeroSimplexes , roomIndex * POINTS_IN_A_ROOM ) )
  listOfSimplexes.extend( translateSimplexIndices( roomOneSimplexes  , roomIndex * POINTS_IN_A_ROOM ) )
  listOfSimplexes.extend( translateSimplexIndices( roomTwoSimplexes  , roomIndex * POINTS_IN_A_ROOM ) )
  listOfSimplexes.extend( translateSimplexIndices( roomThreeSimplexes, roomIndex * POINTS_IN_A_ROOM ) )
# add 1-simplexes of corridors
for corridor in linksData:
  indexRoomSource = encodeCoords(corridor["source"])
  # indexRoomSource = corridor["source"]["x"] * gridSizes["y"] * gridSizes["z"] + corridor["source"]["y"] * gridSizes["z"] + corridor["source"]["z"]
  indexRoomTarget = encodeCoords(corridor["target"])
  # indexRoomTarget = corridor["target"]["x"] * gridSizes["y"] * gridSizes["z"] + corridor["target"]["y"] * gridSizes["z"] + corridor["target"]["z"]
  if corridor["target"]["x"] - corridor["source"]["x"] == 1:  # case + x-corridor
    listOfSimplexes.append( [ indexRoomSource * POINTS_IN_A_ROOM + 1, indexRoomTarget * POINTS_IN_A_ROOM + 0 ] )
  elif corridor["source"]["x"] - corridor["target"]["x"] == 1:  # case - x-corridor
    listOfSimplexes.append( [ indexRoomSource * POINTS_IN_A_ROOM + 0, indexRoomTarget * POINTS_IN_A_ROOM + 1 ] )
  elif corridor["target"]["y"] - corridor["source"]["y"] == 1:  # case + y-corridor
    listOfSimplexes.append( [ indexRoomSource * POINTS_IN_A_ROOM + 3, indexRoomTarget * POINTS_IN_A_ROOM + 2 ] )
  elif corridor["source"]["y"] - corridor["target"]["y"] == 1:  # case - y-corridor
    listOfSimplexes.append( [ indexRoomSource * POINTS_IN_A_ROOM + 2, indexRoomTarget * POINTS_IN_A_ROOM + 3 ] )
  elif corridor["target"]["z"] - corridor["source"]["z"] == 1:  # case + z-corridor
    listOfSimplexes.append( [ indexRoomSource * POINTS_IN_A_ROOM + 5, indexRoomTarget * POINTS_IN_A_ROOM + 4 ] )
  elif corridor["source"]["z"] - corridor["target"]["z"] == 1:  # case - z-corridor
    listOfSimplexes.append( [ indexRoomSource * POINTS_IN_A_ROOM + 4, indexRoomTarget * POINTS_IN_A_ROOM + 5 ] )
  else:
    print("PROBLEM! " + str(corridor["target"]) + ", " + str(corridor["source"]))
  if abs( coordinatesOfPoints[ indexRoomSource * POINTS_IN_A_ROOM + 5 ][2] - coordinatesOfPoints[ indexRoomTarget * POINTS_IN_A_ROOM + 4 ][2] ) > 30:
    print(corridor)
    print( str(indexRoomSource * POINTS_IN_A_ROOM + 5) + ", " + str(indexRoomTarget * POINTS_IN_A_ROOM + 4) )
    print( str(coordinatesOfPoints[indexRoomSource * POINTS_IN_A_ROOM + 5]) + ", " + str(coordinatesOfPoints[indexRoomTarget * POINTS_IN_A_ROOM + 4]) )




# auxiliary function to append lists of values to a string
def stringOfList(vals):
  result = '['
  for val in vals[:-1]:
    result += str(val) + ','
  result += str(vals[-1])
  result += ']'
  return result

# prepare output of mazeModel.json
outModel = '{\n'
outModel += '"numberOfPoints": ' + str( NUMBER_OF_ROOMS * POINTS_IN_A_ROOM ) + ',\n'
outModel += '"coordinatesOfPoints": [\n'
for coords in coordinatesOfPoints[:-1]:
  outModel += '  ' + stringOfList(coords) + ',\n'
outModel += '  ' + stringOfList(coordinatesOfPoints[-1]) + '\n'
outModel += '],\n'
outModel += '"atomNames": [ "p" ],\n'
outModel += '"simplexes": [\n'
for id, simplex in enumerate(listOfSimplexes):
  outModel += '  {\n'
  outModel += '    "id": "s' + str(id) + '",\n'
  outModel += '    "points": ' + stringOfList(simplex) + ',\n'
  outModel += '    "atoms": []\n'
  outModel += '  },\n'
outModel = outModel[:-2] + '\n'
outModel += ']\n'
outModel += '}'


# Write the result on the file mazeModel.json
with open('mazeModel.json', "w") as outputFileModel:
    outputFileModel.write(outModel)



# Collect names of atoms data
atomNames = []
for node in nodesData:
  for atom in node["atoms"]:
    if atom not in atomNames:
      atomNames.append(atom)
# print(atomNames)


# atoms assigned to the rooms
atomEval = {}
for atom in atomNames:
  atomEval[atom] = [False for _ in range(NUMBER_OF_ROOMS)]
# print(atomEval)
for node in nodesData:
  roomIndex = encodeCoords( node["coord"] )
  for atom in node["atoms"]:
    atomEval[atom][roomIndex] = True
# print(atomEval)


# prepare the result for mazeAtoms.json
outAtoms = '{\n'
for atom in atomNames:
  outAtoms += '  "' + atom + '": [\n'
  for val in atomEval[atom]:
    if val:
      outAtoms += '    true,\n' * SIMPLEXES_IN_A_ROOM
    else:
      outAtoms += '    false,\n' * SIMPLEXES_IN_A_ROOM
  outAtoms += '    false,\n' * (NUMBER_OF_CORRIDORS * SIMPLEXES_IN_A_CORRIDOR)
  # for node in nodesData:
  #   if atom in node["atoms"]:
  #     outAtoms += SIMPLEXES_IN_A_ROOM * '    true,\n'
  #   else:
  #     outAtoms += SIMPLEXES_IN_A_ROOM * '    false,\n'
  outAtoms = outAtoms[:-2] + '\n'
  outAtoms += '  ],\n'
# TEST
# outAtoms += '  "rooms": [\n'
# END TEST
outAtoms = outAtoms[:-2] + '\n'
outAtoms += '}'

# print(outAtoms)





# TEST: phony mazeAtom.json
# outAtoms = '{\n'
# outAtoms += '  "prova": [\n'
# for i in range(len(listOfSimplexes)-1):
#   if i%2 == 0:
#     outAtoms += '    true,\n'
#   else:
#     outAtoms += '    false,\n'
# outAtoms += '    true\n'
# outAtoms += '  ]\n'
# outAtoms += '}'

# Write the result on the file mazeAtoms.json
with open('mazeAtoms.json', "w") as outputFileModel:
    outputFileModel.write(outAtoms)




