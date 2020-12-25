import sys
import json
import matplotlib.pyplot as plt



# Read the input file
filename = sys.argv[1]
with open(filename) as f:
  data = json.load(f)

if len(sys.argv) > 2:
    atom = sys.argv[2]
else:
    atom = None



# Visualize using matplotlib

## Retrieve the coordinates of the spatial points
coordinatesOfPoints = data["coordinatesOfPoints"]
X = [coord[0] for coord in coordinatesOfPoints]
Y = [coord[1] for coord in coordinatesOfPoints]
Z = [coord[2] for coord in coordinatesOfPoints]

## Retrieve point-simplexes, edge-simplexes and triangle-simplexes
points, edges, triangles, valuation = {}, {}, {}, {}
for simplex in data["simplexes"]:
    valuation[simplex["id"]] = simplex["atoms"]
    if len(simplex["points"]) == 1:
        points[simplex["id"]] = simplex["points"]
    if len(simplex["points"]) == 2:
        edges[simplex["id"]] = simplex["points"]
    if len(simplex["points"]) == 3:
        triangles[simplex["id"]] = simplex["points"]


## Prepare plt figure
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

## Draw triangles
for id in triangles:
    i,j,k = triangles[id][0], triangles[id][1], triangles[id][2]
    x = [ X[i], X[j], X[k] ]
    y = [ Y[i], Y[j], Y[k] ]
    z = [ Z[i], Z[j], Z[k] ]
    if atom is None:
        ax.plot_trisurf(x, y, z, color="red", alpha=.3)
    elif atom in valuation[id]:
        ax.plot_trisurf(x, y, z, color="green", alpha=.3)
    else:
        ax.plot_trisurf(x, y, z, color="red", alpha=.3)

## Draw edges
for id in edges:
    i,j = edges[id][0], edges[id][1]
    x = [ X[i], X[j] ]
    y = [ Y[i], Y[j] ]
    z = [ Z[i], Z[j] ]
    if atom is None:
        ax.plot(x, y, z, color='blue', alpha=.7, linewidth=2, markersize=0)
    elif atom in valuation[id]:
        ax.plot(x, y, z, color='green', alpha=.7, linewidth=2, markersize=0)
    else:
        ax.plot(x, y, z, color='red', alpha=.7, linewidth=2, markersize=0)

## Draw points
for id in points:
    i = points[id][0]
    x, y, z = X[i], Y[i], Z[i]
    if atom is None:
        ax.plot(x, y, z, color='green', alpha=1, linewidth=0, marker='o', markersize=5)
    elif atom in valuation[id]:
        ax.plot(x, y, z, color='green', alpha=1, linewidth=0, marker='o', markersize=5)
    else:
        ax.plot(x, y, z, color='red', alpha=1, linewidth=0, marker='o', markersize=5)

plt.show()

