def parseLine(line):
    """
    Parses a line and returns a map of the claim properties.

    >>> parseLine("#1 @ 829,837: 11x22")
    {'id': '1', 'x': 829, 'y': 837, 'width': 11, 'height': 22}
    >>> parseLine("#34 @ 35,270: 11x25")
    {'id': '34', 'x': 35, 'y': 270, 'width': 11, 'height': 25}
    """
    parts = line.split(' ')
    id = parts[0].replace('#', '')
    startCoordinatesParts = parts[2].split(',')
    startXCoordinate = int(startCoordinatesParts[0])
    startYCoordinate = int(startCoordinatesParts[1].replace(':', ''))
    sizeParts = parts[3].split('x')
    width = int(sizeParts[0])
    height = int(sizeParts[1])
    return {"id": id, "x": startXCoordinate, "y": startYCoordinate, "width": width, "height": height}

def getPointsOfClaim(claim):
    """
    Returns a list of points that the clain covers

    >>> getPointsOfClaim({'id': '34', 'x': 35, 'y': 270, 'width': 2, 'height': 2})
    [(35, 270), (35, 271), (36, 270), (36, 271)]
    >>> getPointsOfClaim({'id': '34', 'x': 829, 'y': 837, 'width': 3, 'height': 1})
    [(829, 837), (830, 837), (831, 837)]
    """
    points = []
    for i in range(claim['width']):
        for j in range(claim['height']):
            points.append((claim['x'] + i, claim['y'] + j))
    return points

def challenge1(filename):
    """
    Return the number of points that have more than one claim on them.

    >>> challenge1('input.txt')
    104126
    """
    materialClaims = [[0] * 2000 for x in range(2000)]
    with open(filename) as f:
        for line in f:
            claim = parseLine(line.rstrip())
            points = getPointsOfClaim(claim) 
            for x,y in points:
                materialClaims[x][y] += 1

    numOverlappingPoints = len([cell for row in materialClaims for cell in row if cell > 1])
    return numOverlappingPoints

def challenge2(filename):
    """
    Return the number of points that have more than one claim on them.

    >>> challenge2('input.txt')
    '695'
    """
    materialClaims = [[[] for x in range(2000)] for x in range(2000)]
    claims = []
    with open(filename) as f:
        for line in f:
            claim = parseLine(line.rstrip())
            claims.append(claim)

            points = getPointsOfClaim(claim) 
            for x,y in points:
                materialClaims[x][y].append(claim['id'])

    notOverlappingClaim = -1
    for claim in claims:
        points = getPointsOfClaim(claim) 
        overlaps = [materialClaims[x][y] for x,y in points if len(materialClaims[x][y]) != 1]
        if overlaps == []:
            notOverlappingClaim = claim['id'] 
            break
    return notOverlappingClaim

print(challenge2('input.txt'))

if __name__ == "__main__":
    import doctest
    doctest.testmod()