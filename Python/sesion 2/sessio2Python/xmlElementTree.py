#!/usr/bin/python

import xml.etree.ElementTree as ET

#root = ET.fromstring(moviedata)
tree = ET.parse('movies.xml')
root = tree.getroot()

def showAll(root,blanks):
    print blanks, root.tag, root.attrib, root.text
    for child in root:
        showAll (child,blanks+"  ")

#print root.tag
#print root.attrib

showAll(root,"")

# tots els 'year' que son fills de 'movie' fills del nivell principal
print 'movies with more than 5 stars'
for movie in root.findall('movie'):
    stars = int(movie.find('stars').text)
    if stars > 5:
        print movie.get('title'),stars


# elements a nivell superior
#root.findall(".")

# nodes 'year' que son fills de nodes amb title='Transformers'
for tra in root.findall("*[@title='Transformers']/year"):
    print tra.text

