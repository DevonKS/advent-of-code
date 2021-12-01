import math
from scipy import stats

def manhattenDistance(pointA, pointB):
    return abs(pointA[0] - pointB[0]) + abs(pointA[0] - pointB[0])

def challenge1(filename):
    """
    Challenge 1 (Answer should be less than 11476)

    >>> challenge1("input.txt")
    5
    """
    points = []
    with open(filename) as f:
        for line in f:
            parts = line.split(", ")
            point = (int(parts[0]), int(parts[1]))
            points.append(point)
    
    minX = min(points, key=(lambda item: item[0]))[0]
    minY = min(points, key=(lambda item: item[1]))[1]
    maxX = max(points, key=(lambda item: item[0]))[0]
    maxY = max(points, key=(lambda item: item[1]))[1]
    xRange = range(minX, maxX + 1)
    yRange = range(minY, maxY + 1)
    grid = [[-1 for _ in yRange] for _ in xRange]

    for x in xRange:
        for y in yRange:
            minPointIndex = -1
            minPointValue = math.inf
            for index,point in enumerate(points):
                dist = manhattenDistance((x,y), point)
                if dist == minPointValue:
                    break
                elif  dist < minPointValue:
                    minPointIndex = index
                    minPointValue = dist
            grid[x - minX][y - minY] = minPointIndex
    gridMode = stats.mode([cell for row in grid for cell in row])
    print("Min Point: (%i, %i)" % (minX, minY))
    print("Max Point: (%i, %i)" % (maxX, maxY))
    print("Winning Point: ", points[gridMode.mode[0]])
    print(gridMode)
    return gridMode.count[0]

challenge1("input.txt")

if __name__ == "__main__":
    import doctest
    doctest.testmod()