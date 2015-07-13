import cv2
import numpy as np
import sys

fn = sys.argv[1]
#fn = "test2.jpg"

def show(src, lines = [], pairs = [], corners = [], rects = [], table = None, cmap = None, small=True, writePath=None, format=".jpg", title = 'image'):
    img = src.copy()
    for rho,theta in lines:
        a = np.cos(theta)
        b = np.sin(theta)
        x0 = a*rho
        y0 = b*rho
        x1 = int(x0 + 5000*(-b))
        y1 = int(y0 + 5000*(a))
        x2 = int(x0 - 5000*(-b))
        y2 = int(y0 - 5000*(a))
        cv2.line(img,(x1,y1),(x2,y2),(0,0,255),2)

    if table is not None:
        drawTable(img, table)

    for (x,y) in [p for p in pairs if p]:
        onSpot = False
        if cmap is not None:
            onSpot = (cmap[y,x] != 0)
        else:
            onSpot = True
        color = (0,0,255) if onSpot else (0,255,255)
        radius = 2 if cmap else 4
        cv2.circle(img,(x,y),radius,color,-1)

    for c in corners:
        if c is not None:
            drawCorner(img, c)

    for (p1,p2) in rects:
        cv2.rectangle(img, p1, p2, (0,100,200), 2)

    if small:
        img = cv2.resize(img, (0,0), fx=0.50, fy=0.50)

    if writePath is None:
        cv2.imshow(title,img)
        cv2.waitKey(0)
        cv2.destroyAllWindows()
    else:
        cv2.imwrite(writePath+format, img)

img = cv2.imread(fn, cv2.CV_LOAD_IMAGE_COLOR)
gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)

gray = gray.tolist()

print gray
