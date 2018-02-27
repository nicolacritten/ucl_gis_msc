#Import the needed modules
import math
import numpy as np
import csv
import os
import matplotlib.pyplot as plt
from matplotlib.path import Path
import matplotlib.patches as mpatches
from matplotlib.font_manager import FontProperties

#Set working directory (where the files are located)
wd = os.chdir("/Users/Documents/UNI/MASTERS/082 - GIS Principles and Technology/Programming/Coursework/PIP/")

#Set file names for the polygon and point coordinate files
polyfile = "testPoly.csv"
pointsfile = "testPoints.csv" #the points file must have a header line for the code to work correctly

#Geometry class
class Geom(object):
    def getStartPoint(self):
        return self.coords[0]

    def getEndPoint(self):
        return self.coords[-1]

    def getNumPoints(self):
        return self.coords.shape[0]

    def addPoint(self, point):
        self.coords = np.vstack([self.coords, point])

#Point class
class Point(Geom):
    def __init__(self, x=0, y=0, z=float('nan'), colour=""):
        self.__coords = np.array([x, y, z], dtype=float)
        self.__coords.shape = (1, 3)
        self.__colour = "grey"

    @property
    def x(self):
        return self.__coords[0, 0]

    @property
    def y(self):
        return self.__coords[0, 1]

    @property
    def z(self):
        return self.__coords[0, 2]
    
    @property
    def colour(self):
        return self.__colour
    
    @colour.setter
    def colour(self, colour):
        self.__colour = colour

    @x.setter
    def x(self, x):
        self.__coords[0, 0] = x

    @y.setter
    def y(self, y):
        self.__coords[0, 1] = y

    @z.setter
    def z(self, z):
        self.__coords[0, 2] = z

    @property
    def coords(self):
        return self.__coords

    def addPoint(self, point):
        return "Can't add a point to a point"

#Line class
class Line(Geom):
    def __init__(self, points=None):
        if points is None:
            self.__coords = None
        else:
            self.__coords = np.vstack(points)

    @property
    def coords(self):
        return self.__coords

    @coords.setter
    def coords(self, points):
            self.__coords = np.vstack(points)

    def addPoint(self, point):
        if self.__coords is None:
            self.__coords = point
            self.__coords.shape = (1, 3)
        else:
            self.__coords = np.vstack([self.__coords, point])

#Polygon class
class Polygon(Line, Geom):
    def getEndPoint(self):
        return self.getStartPoint()

#Bounding box class
#Inspiration code: https://techoverflow.net/2017/02/23/computing-bounding-box-for-a-list-of-coordinates-in-python/
class BoundingBox(object):
    def __init__(self, points):

        #Set the min and max as infinity for comparison
        self.__minx = float("inf")
        self.__miny = float("inf")
        self.__maxx = float("-inf")
        self.__maxy = float("-inf")
        
        for x, y in points:
            
            # Set minimum coords (compares to find smallest)
            if x < self.__minx: #if the given number is smaller than the current minimum
                self.__minx = x #then set it as the current minimum value
            if y < self.__miny:
                self.__miny = y
                
            # Set maximum coords (compares to find largest)
            if x > self.__maxx: #if the given number is larger than the current maximum
                self.__maxx = x #then set it as the current maximum value
            elif y > self.__maxy:
                self.__maxy = y

    @property
    def coords(self):
        return [(self.__minx, self.__miny), (self.__maxx,self.__miny),
                (self.__maxx,self.__maxy), (self.__minx,self.__maxy)]
        
    @property
    def minx(self):
        return self.__minx
    
    @property
    def miny(self):
        return self.__miny
    
    @property
    def maxx(self):
        return self.__maxx
    
    @property
    def maxy(self):
        return self.__maxy    


#Determine if a point is inside the bounding box or not
def pibb(x, y, bbcoords): #x and y coordinates of the point, a list of bounding box coordinates
    
    insidebb = False #initially assigns false
    
    b1x,b1y = bbcoords[0] #assigns the min coordinate pair
    b2x,b2y = bbcoords[2] #assigns the max coordinate pair
    
    if b1x <= x and x <= b2x: #is the point x coordinate within the bounding box x coordinates
        if b1y <= y and y <= b2y: #is the point y coordinate within the bounding box y coordinates
            insidebb = not insidebb #if it is within the coordinate pair then assigns True
    
    if insidebb: return "withinbb"
    else: return "outsidebb"


#Determine if a point is inside the polygon or not using the Ray Casting Method
#Also considers vertex cases
#Inspiration code: http://geospatialpython.com/2011/01/point-in-polygon.html
def pip(x, y, polycoords): #x and y coordinates of the point, a list of polygon coordinates
    
    n = len(polycoords) #number of points for the polygon
    
    #checks if the point is on a vertex
    counter = 0
    for item in polycoords: #iterates through each polygon coordinate pair
        px,py = polycoords[counter] #assigns the coordinates
        if (x,y) == (px,py): #if the point coordinate is the same as the polygon vertex coordinate
            return "vertex" #it is located on a vertex
        counter = counter + 1
                
    insidep = False #initially assigns false

    p1x,p1y = polycoords[0] #assigns the first coordinate pair
    
    for i in range(n+1): #iterates around each polygon point
        p2x,p2y = polycoords[i % n] #assigns the coordinates
        if y > min(p1y,p2y): #if the point y coordinate is bigger than the minimum polygon y coordinate
            if y <= max(p1y,p2y): #if the point y coordinate is smaller or equal to the max polygon y coordinate
                if x <= max(p1x,p2x): #if the point x coordinate is bigger or equal to the maximum polygon x coordinate
                    if p1y != p2y: #if the y coordinates are not equal
                        xints = (y-p1y)*(p2x-p1x)/(p2y-p1y)+p1x #the number of times it crosses before leaving the bounding box
                    if p1x == p2x or x <= xints:
                        insidep = not insidep #if it is within then assigns True
        p1x,p1y = p2x,p2y #assigns the second coordinate pair as the first    
      
    if insidep: return "withinp"
    else: return "outsidep"

#Checks if the point is on a polygon boundary
def boundary(x,y,polycoords):
    
    n = len(polycoords) #number of points for the polygon
        
    #Inspiration code: https://stackoverflow.com/questions/17749401/python-find-if-point-lay-on-the-border-of-a-polygon  
    for i in range(n):
        p1x, p1y = polycoords[i] #assigns the coordinates
        p2x, p2y = polycoords[(i + 1) % n] 
        v1x = p2x - p1x #the line between the two polygon verticies
        v1y = p2y - p1y
        v2x = x - p1x #the line between the polygon vertex and the point coordinate
        v2y = y - p1y
        if(v1x * v2y - v1y * v2x == 0): #if the two lines are parallel 
            if(v2x / v1x > 0): #if the two lines are in the same direction
                if(v1x * v1x + v1y * v1y >= v2x * v2x + v2y * v2y): #if the polygon to point line is shorter than the polygon only line
                    return "boundary" #it is located on a line, therefore, boundary
        
        
#Determine if a point is inside the polygon or not using the functions above
#Prints the outcome and assigns the colour of the point
#takes a Point, Bounding Box, and Polygon objects, and a number (to define which point)
def isinside(point, bbox, poly, counter):
    
    returned = pibb(point.x, point.y, bbox.coords) #tests if inside bounding box
    
    if returned == "outsidebb": #if not inside the bounding box
        print "Point %(counter)d : Outside bounding box" % {"counter": counter}
        point.colour = "red"
    
    else: #if inside the bounding box
                
        returned2 = boundary(point.x, point.y, poly.coords)
        returned = pip(point.x, point.y, poly.coords) #test if inside the polygon (including vertex and boundary)

        
        if returned2 == "boundary": #boundary case
            print "Point %(counter)d : Polygon boundary" % {"counter": counter}
            point.colour = "purple"

        elif returned == "outsidep": #outside case
                print "Point %(counter)d : Outside the polygon" % {"counter": counter}
                point.colour = "orange"
            
        else: #inside case
                print "Point %(counter)d : Inside polygon" % {"counter": counter}
                point.colour = "green"
            
        if returned == "vertex": #vertex case
            print "Point %(counter)d : Polygon vertex" % {"counter": counter}
            point.colour = "blue"



#Draws a plot showing the polygon, bounding box and points
#The points will be coloured according to their location
#Inspiration code: https://matplotlib.org/users/path_tutorial.html

def drawplot(poly, bbox, pointsList, patches):
    
    fig = plt.figure() #creates a figure object
    ax = fig.add_subplot(111) #creates an axis within the figure object
    
    #draws the polygon
    noOfPoints = poly.getNumPoints() #Gets number of points in the polygon  
    codes = [] #creates an empty list for adding the path codes
    
    for number in range(noOfPoints):
        codes.append(Path.LINETO) #iterates through the points in the polygon adding the path code to draw a line
    
    codes[0] = Path.MOVETO #gives the code to pick up the pen for the first point
    codes[-1] = Path.CLOSEPOLY #gives the code to complete the polygon for the last point
    
    path = Path(poly.coords, codes) #creates the polygon shape path    
    patch = patches.PathPatch(path, facecolor='yellow', lw=2, label='Polygon') #defines the patch to be added for the polygon shape - gives colour and linewidth
    
    ax.add_patch(patch) #adds the polygon shape to the axis

    
    #draws the points
    #iterates around the list of Point objects plotting each Point in turn  
    counter = 0
    for item in points:
        colour = points[counter].colour #gets the colour of the Point
        plt.plot([int(points[counter].x)], [int(points[counter].y)], 
                  'ro', color = colour, markersize = 8) #plots the points in the colour provided
        counter = counter + 1
    
    
    #draws the bounding box
    bbverts = bbox.coords #puts the bounding box verticies in a new list
    bbverts.append((0,0)) #adds a blank vertex to the list, which is ignored when drawing the path
    
    bbcodes = [Path.MOVETO,
             Path.LINETO,
             Path.LINETO,
             Path.LINETO,
             Path.CLOSEPOLY] #creates a list with the codes to draw the bounding box
    
    bbpath = Path(bbverts, bbcodes) #creates the bounding box shape path 
    bbpatch = patches.PathPatch(bbpath, facecolor='none', lw=2, edgecolor='grey') #defines the patch to be added for the bounding box shape - gives colour and linewidth
    
    ax.add_patch(bbpatch) #adds the bounding box to the axis
    
    #edits the axis properties
    ax.set_xlim(bbox.minx - 8,bbox.maxx + 8) #sets the axis limits bigger than the bounding box size
    ax.set_ylim(bbox.miny - 8,bbox.maxy + 8)
    
    ax.set_xlabel('X Coordinates') #sets the axis labels
    ax.set_ylabel('Y Coordinates')
    ax.set_title("Plot Showing the Polygon and Points", fontsize='large') #sets a title for the plot
        
    #draws the legend
    colours = ["green", "red", "orange", "blue", "purple"]
    text = ["Inside Polygon", "Outside Bounding Box", "Outside Polygon", "On a Vertex", "On a Boundary"]
    patches = [ plt.plot([],[], marker="o", ms=8, ls="", mec=None, color=colours[i], 
                label="{:s}".format(text[i]) )[0]  for i in range(len(text)) ]
    font = FontProperties()
    font.set_size('small')
    plt.legend(handles=patches, loc='lower center', ncol=2, numpoints=1, prop=font)
    
    return plt #returns the plot


#Open the polygon coordinates CSV file
try:
    with open(polyfile) as csvfile:
        
        #Show an error messsage and stop executing if the provided Polygon file is empty.
        if (os.stat(polyfile).st_size == 0):
            raise SystemExit('The Polygon file is empty. Please check the file and add polygon coordinates.')
        
        else:
            polyreader = csv.reader(csvfile)
            coords = []
            
        for row in polyreader:
            coords.append([int(row[0]),int(row[1])])
            
except IOError: #stops excuting and shows an error message if the file can't be opened
    raise SystemExit("The polygon file could not be opened. Check the file, file path or name.")

#Creates a Polygon object using the coordinates from the CSV file
poly = Polygon(coords)

#Creates a bounding box object around the polygon using the coordinates from the CSV file
bbox = BoundingBox(coords)


#Open the points coordinates CSV file and creates Point objects
try:
    with open(pointsfile) as ptcsvfile:
        
        ptcsvfile.readline() #reads the first line to skip as its a header
        pointreader = csv.reader(ptcsvfile)
        points = []
            
        for row in pointreader:        
            points.append(Point(int(row[0]), int(row[1]))) #adds a new Point item to a list
        
        if len(points) == 0: #shows a message if the file given is empty so no Points can be made
            print "No point coordinates provided by the file. Please add some below."
            
except IOError: #stops excuting and shows an error message if the file can't be opened
    raise SystemExit("The points file could not be opened. Check the file, file path or name.")


#Calls 'isinside' to get the location of the points and print the outcome
count = 1
returned = ""
for item in points: #iterates around all the given points
    isinside(points[count-1], bbox, poly, count)
    count = count + 1


#Calls 'drawplot' to draw the plot showing the polygon, bounding box and points
poly.coords = np.vstack([poly.coords, [0,0]]) #adds a blank vertex to the list, which is ignored when drawing the path 
plot = drawplot(poly, bbox, points, mpatches)
plot.show()


#Asks if the user wants to try a new point, calculates if that is inside the polygon and then draws a new plot including that point
pointno = 0
while True:
    addpoint = raw_input("Try a new point? (enter y or n):") #asks user if they want to try a new point

    if addpoint == "y": #if they input 'y' then continue
        pointno = pointno + 1
        
        try:
            newxcoord = int(raw_input("Enter the x coordinates (enter one number):")) #asks user to enter the x coordinate
        except ValueError: #if they do not enter an integer then the original question will return
            print("Incorrect format. Start again.") #prints error message
            continue
        
        try:
            newycoord = int(raw_input("Enter the y coordinates (enter one number):")) #asks user to enter the y coordinate
        except ValueError: #if they do not enter an integer then the original question will return
            print("Incorrect format. Start again.") #prints error message
            continue
        
        newpoint = Point(newxcoord,newycoord) #creates a new Point object with the user inputted coordinates
        
        isinside(newpoint, bbox, poly, pointno) #calls 'isinside' to get the location of the point and print the outcome
        
        points.append(newpoint) #adds the new point to the list of points
        
        plot = drawplot(poly, bbox, points, mpatches) #calls 'drawplot' to redraw the plot with the new point added
        plot.show()
        
    elif addpoint == "n": #if the user inputes 'n'
        print 'Ok, no worries.' #print message
        break #ends loop
    
    else: print 'Incorrect entry. Try again.' #if the user input is the wrong format then error message is printed and original question will return