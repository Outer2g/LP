#!/usr/bin/python

import urllib
import xml.etree.ElementTree as ET

sock = urllib.urlopen("http://wservice.viabicing.cat/v1/getstations.php?v=1")
xmlSource = sock.read()
sock.close()

root = ET.fromstring(xmlSource)

def showAll(root,blanks):
    print blanks, root.tag, root.attrib, root.text
    for child in root:
        showAll (child,blanks+"  ")

class Bicing:
    def __init__(self,xmlSource):
        self.root = ET.fromstringlist(xmlSource)
    def getTotalBicis(self):
        suma = 0
        for station in self.root.findall('station'):
            n = int(station.find('bikes').text)
            suma+=n
        return suma
bici = Bicing(xmlSource)
print 'numero de bicis total', bici.getTotalBicis()

print 'numero estacions amb alguna bici lliure'
suma = 0
for station in root.findall('station'):
    n = int(station.find('bikes').text)
    if n > 0: suma+=1
print suma