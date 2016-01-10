#!/usr/bin/python

import urllib
import xml.etree.ElementTree as ET
import sys

class Esdeveniments:
    def __init__(self,xmlSource):
        self.root = ET.fromstringlist(xmlSource).find('body').find('resultat')

#donat un nom, busquem els noms d'actes, llocs o barris que coincideixen amb el nom
    def busca(self,nom):
        llista = []
        for acte in self.root.find('actes').findall('acte'):
            nomActe = acte.find('nom').text
            nomLloc = acte.find('lloc_simple').find('nom').text
            nomBarri = acte.find('lloc_simple').find('adreca_simple').find('barri').text
            if (nomActe == nom or nomLloc == nom or nomBarri == nom):
                    llista.extend(acte)
        return llista


sock = urllib.urlopen("http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=199")
xmlSource = sock.read()
sock.close()

esdeveniments = Esdeveniments(xmlSource)
aux = sys.argv[1][1:]
n = len(aux)
aux = aux[:n-1]
print aux
L = esdeveniments.busca(aux)
print len(L)
