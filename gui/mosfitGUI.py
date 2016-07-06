#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
from Tkinter import *
import tkFileDialog 

########################################################################
class BarreMenu(Frame):
    def __init__(self, parent):
        Frame.__init__(self, parent)   
        self.parent = parent        
        self.initUI()

    def initUI(self):
      
        self.pack(fill=BOTH, expand=1)

        menubar = Menu(self.parent)
        self.parent.config(menu=menubar)

        fileMenu = Menu(menubar)
        fileMenu.add_command(label="Ouvrir", command=self.onOpen)
        fileMenu.add_separator()
        fileMenu.add_command(label="Exit", command=self.parent.quit)        
        menubar.add_cascade(label="Fichier", menu=fileMenu)        

    def onOpen(self):
        ftypes = [('.coo', '*.coo'), ('All files', '*')]
        dlg = tkFileDialog.Open(self, filetypes = ftypes)
        fl = dlg.show()

        if fl != '':
            text = self.readFile(fl)
            try:
              self.parent.txt.delete(0.0,END)
              self.parent.txt.insert(0.0, text)
            except Exception:
              pass
    def readFile(self, filename):
        f = open(filename, "r")
        text = f.read()
        return text
########################################################################
class Data():
  
  def __init__(self,parent):
    
    self.parent=parent
    try:
      self.parent.txt.delete(0.0,END)
    except:
      pass
    self.parent.txt=Text(parent)
    self.CN=0.078125
    self.NMAX=20
    self.NS=8
    self.NS1=5
    self.NS2=8
    self.IZZ=1
    self.IOPT=1
    self.HBRUIT=0.0 
    self.IZ=[0, 0, 0, 0, 0, 0, 0, 0, 0, 0] #10
    self.IO=[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] #20
    self.PLAGEL=[0, 0]
    self.GRASS=[0, 0, 2, 5, 0, 0, 0, 0, 0, 0]#10
    #sous Spectres    
    self.setSpectres()
    self.textFromEntries()
    #~ self.parent.txt.pack(fill=BOTH, expand=1)
    
  def textFromVariables(self):
    #transfert data form variables
    try:
      self.parent.txt.delete(0.0,END)
    except:
      pass
    self.setText("titre\n")
    #Variables generales
    string= str(self.CN)+" "+str(self.NMAX)
    string=string+" "+str(self.NS)+" "+str(self.NS1)+" "+str(self.NS2)
    string=string+" "+str(self.IZZ)+" "+str(self.IOPT)+" "+str(self.HBRUIT)
    self.parent.txt.insert(INSERT,string)
    #canaux a ignorer
    self.insertTableOption(self.IZZ,1,self.IZ)
    #Options IO
    self.insertTableOption(self.IOPT,1,self.IO)
    #Plage de canaux à lisser
    self.insertTableOption(self.IO[12],3,self.PLAGEL)
    #Plages de canaux a tracer
    self.insertTableOption(self.IO[16],1,self.GRASS)
    #Spectres
    i=0
    while i<self.NS:
      self.insertSpectre(i)
      i+=1
    #Inserer spectre ?
    #Inserer spectre de bruit ?
    
  def setText(self,string):
    
    self.parent.txt.insert(INSERT,string)
      
  def insertTableOption(self, option,valOption, table):
    
    if option==valOption:
      string="\n"
      for i in table:
        string=string+str(i)+" "
      self.parent.txt.insert(INSERT,string)
      
  def insertSpectre(self,i):
    
    string="\n"
    if self.sousSpectres[i].kind==0:
      string+=str(self.sousSpectres[i]["DI"])
      string+=" "
      string+=str(self.sousSpectres[i]["GA"])
      string+=" "
      string+=str(self.sousSpectres[i]["H1"])
      string+=" "
      string+=str(self.sousSpectres[i]["SQ"])
      string+=" "
      string+=str(self.sousSpectres[i]["CH"])
      string+=" "
      string+=str(self.sousSpectres[i]["ETA"])
      string+=" "
      string+=str(self.sousSpectres[i]["THETA"])
      string+=" "
      string+=str(self.sousSpectres[i]["GAMMA"])
      string+=" "
      string+=str(self.sousSpectres[i]["BETA"])
      string+=" "
      string+=str(self.sousSpectres[i]["ALPHA"])
      string+=" "
      string+=str(self.sousSpectres[i]["MONOC"])
      string+="\n"
      j=0
      while j<10:
        string=string+str(self.sousSpectres[i].NB[j])+" "
        j+=1
      string+=str(self.sousSpectres[i]["IOGV"])
      if self.sousSpectres[i]["IOGV"]==3:
        string+="\n"
        j=0
        while j<8:
          string=string+str(self.sousSpectres[i].GV[j])+" "
          j+=1
        string+="\n"
        j=0
        while j<8:
          string=string+str(self.sousSpectres[i].NG[j])+" "
          j+=1
    elif self.sousSpectres[i].kind==1:
      string+=str(self.sousSpectres[i]["DI0"])
      string+=" "
      string+=str(self.sousSpectres[i]["PDI"])
      string+=" "
      string+=str(self.sousSpectres[i]["GA"])
      string+=" "
      string+=str(self.sousSpectres[i]["H1"])
      string+=" "
      string+=str(self.sousSpectres[i]["SQ0"])
      string+=" "
      string+=str(self.sousSpectres[i]["PSQ"])
      string+=" "
      string+=str(self.sousSpectres[i]["CH0"])
      string+=" "
      string+=str(self.sousSpectres[i]["PCH"])
      string+=" "
      string+=str(self.sousSpectres[i]["ETA0"])
      string+=" "
      string+=str(self.sousSpectres[i]["THETA0"])
      string+=" "
      string+=str(self.sousSpectres[i]["PTHETA"])
      string+=" "
      string+=str(self.sousSpectres[i]["GAMMA0"])
      string+=" "
      string+=str(self.sousSpectres[i]["BETA0"])
      string+=" "
      string+=str(self.sousSpectres[i]["ALPHA0"])
      string+=" "
      string+=str(self.sousSpectres[i]["MONOC0"])
      string+="\n"
      j=0
      while j<10:
        string=string+str(self.sousSpectres[i].NB0[j])+" "
        j+=1
    else:
      string=''
    self.parent.txt.insert(INSERT,string)
    
  def setSpectres(self):
    
    # creation de la liste des sous-spectres
    self.sousSpectres=[]
    i=0
    while i<self.NS:
      if i<self.NS1-1 :
        self.sousSpectres.append(Spectre(0))
      elif i<self.NS2:
        self.sousSpectres.append(Spectre(i-self.NS1+2))
      else:
        self.sousSpectres.append(Spectre(0))
      i+=1
  def 
  
  def convertFromCoo(self):
    
    #read data from text
    pass
#######################################################################
class Spectre():
  def __init__(self,kind):
    self.kind = kind #0 = sous-spectre standard, 1= premier sous spectre d'une distrib, 2 = 2eme, etc..
    self.parameters=dict()
    if self.kind==0:
      self["DI"]=0
      self["GA"]=0.1
      self["H1"]=0
      self["SQ"]=0
      self["CH"]=0
      self["ETA"]=0
      self["THETA"]=0
      self["GAMMA"]=0
      self["ALPHA"]=0
      self["BETA"]=0
      self["MONOC"]=0
      self["IOGV"]=2
      self.NB=[0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 
      self.GV=[0, 0, 0, 0, 0, 0, 0, 0] 
      self.NG=[0, 0, 0, 0, 0, 0, 0, 0] 
    else:
      self["DI0"]=0
      self["PDI"]=0
      self["GA"]=0.1
      self["H1"]=0
      self["SQ0"]=0
      self["PSQ"]=0
      self["CH0"]=0
      self["PCH"]=0
      self["ETA0"]=0
      self["PTHETA"]=0
      self["THETA0"]=0
      self["GAMMA0"]=0
      self["ALPHA0"]=0
      self["BETA0"]=0
      self["MONOC0"]=0
      self.NB0=[0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 
  def __setitem__(self,index,value):
    self.parameters[index]=value
  def __getitem__(self, index):
    return self.parameters[index]
#######################################################################
def main():

  fenetre = Tk()
  fenetre.title("Mosfit2016")
  fenetre.geometry("600x500+300+300")
  p = PanedWindow(fenetre, orient=HORIZONTAL)
  p.pack(side=TOP, expand=Y, fill=BOTH, pady=2, padx=2)
  p2= PanedWindow(p, orient=VERTICAL)
  p2.pack(side=TOP, expand=Y, fill=BOTH, pady=2, padx=2)
  barre = BarreMenu(fenetre)
  donnees=Data(fenetre)

  p.add(Label(p, text='Volet inputs', background='white', anchor=CENTER) )
  p.add(p2)
  b1=Button(p2, text="Convert to .coo",command=donnees.textFromVariables)
  p2.add(b1)
  b1.pack(fill=Y)
  b2=Button(p2, text="interpret .coo")
  p2.add(b2)
  b2.pack(fill=Y)
  p.add(fenetre.txt)

  fenetre.mainloop()  
if __name__ == '__main__':
    main()  
