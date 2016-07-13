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
      
        self.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)

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
class DataEntries(PanedWindow):
  def __init__(self,master,parent):
    PanedWindow.__init__(self,master)
    self.parent=parent
    #~ self.ligne1=PanedWindow(self,orient=HORIZONTAL)
    #~ self.ligne2=PanedWindow(self,orient=HORIZONTAL)
    #~ self.ligne3=PanedWindow(self,orient=HORIZONTAL)
    self.etitre=Entry(self,width=30)
    self.eCN=Entry(self,width=10)
    self.eNMAX=Entry(self,width=4)
    self.add(self.eCN)
    self.add(self.eNMAX)
    
    
    self.eNS=Entry(self,width=2)
    self.eNS1=Entry(self,width=2)
    self.eNS2=Entry(self,width=2)
    self.add(self.eNS)
    self.add(self.eNS1)
    self.add(self.eNS2)
    #~ vcmd=self.register(self.checkIZZ)
    #~ self.eIZZ=Entry(self,width=2,validate=ALL,validatecommand=vcmd)
    self.eIZZ=Entry(self,width=2,validate=ALL)
    self.eIOPT=Entry(self,width=2)
    self.eHBRUIT=Entry(self,width=8)
    # Options
    self.eIO =[]
    i=0
    while i<20:
      self.eIO.append(Entry(self,width=2))
      i+=1
    # Canaux a ignorer
    self.eIZ=[]
    i=0
    while i<10:
      self.eIZ.append(Entry(self,width=4))
      i+=1
    self.ePLAGEL=[]
    for i in [0,1]:
      self.ePLAGEL.append(Entry(self,width=4))
    i=0
    self.eGRASS=[]
    while i<10:
      self.eGRASS.append(Entry(self,width=4))
      i+=1
    
  def pack(self,**options):
    # affichages
    PanedWindow.pack(self,options)
    Label(self, text="Titre").grid(row=0);self.etitre.grid(row=0,column=1,columnspan=5,sticky=W)
    Label(self, text="CN").grid(row=1);self.eCN.grid(row=1,column=1,columnspan=3,sticky=W)
    Label(self, text="NMAX").grid(row=1,column=4);self.eNMAX.grid(row=1,column=5,sticky=W)
    Label(self, text="NS").grid(row=2);self.eNS.grid(row=2,column=1,sticky=W)
    Label(self, text="NS1").grid(row=2,column=2);self.eNS1.grid(row=2,column=3,sticky=W)
    Label(self, text="NS2").grid(row=2,column=4);self.eNS2.grid(row=2,column=5,sticky=W)
    Label(self, text="IZZ").grid(row=3);self.eIZZ.grid(row=3,column=1,sticky=W)
    Label(self, text="IOPT").grid(row=3,column=2);self.eIOPT.grid(row=3,column=3,sticky=W)
    Label(self, text="HBRUIT").grid(row=3,column=4);self.eHBRUIT.grid(row=3,column=5,sticky=W)
    Label(self, text="Options").grid(row=4,columnspan=2)
    i=0
    while i<20:
      Label(self, text="IO("+str(i+1)+")").grid(row=5+i ,sticky=E)
      self.eIO[i].grid(row=5+i,column=1,sticky=W)
      i+=1
    Label(self, text="Canaux a ignorer :").grid(row=4, column=3, columnspan=3,sticky=W)
    i=0
    while i<5:
      self.eIZ[2*i].grid(row=5+i,column=3,sticky=W)
      Label(self, text=" a ").grid(row=5+i,column=4)
      self.eIZ[2*i+1].grid(row=5+i,column=5,sticky=W)
      i+=1
    Label(self, text="Spectes a tracer:").grid(row=4, column=6, columnspan=4,sticky=W)
    i=0
    while i<5:
      self.eGRASS[2*i].grid(row=5+i,column=6,sticky=W)
      Label(self, text=" a ").grid(row=5+i,column=7)
      self.eGRASS[2*i+1].grid(row=5+i,column=8,sticky=W)
      i+=1
    Label(self, text="Spectres a lisser :").grid(row=11, column=3, columnspan=3,sticky=W)
    self.ePLAGEL[0].grid(row=12,column=3,sticky=W)
    Label(self, text=" a ").grid(row=12, column=4)
    self.ePLAGEL[1].grid(row=12,column=5,sticky=W)
  #~ def checkIZZ(self):
    #~ try:
      #~ if int(self.eIZZ.get())==0 or int(self.eIZZ)==1:
        #~ if int(self.eIZZ.get())!=1:
          #~ i=0
          #~ print("zou")
          #~ while i<10:
            #~ self.eIZ[i].config(bg="red")
          #~ return True
      #~ else:
        #~ return False
    #~ except:
      #~ return True
########################################################################
class Data():
  
  def __init__(self,parent,textmaster,):
    
    self.parent=parent
    self.parent.txt=Text(textmaster)
    self.l=0 #derniere ligne lue dans le texte
    self.titre="Sans Titre"
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
    self.textFromVariables()
    #~ self.parent.txt.pack(fill=BOTH, expand=1)
    
  def textFromVariables(self):
    #transfert data form variables
    try:
      self.parent.txt.delete(0.0,END)
    except:
      pass
    self.setText(self.titre+"\n")
    #Variables generales
    string= str(self.CN)+" "+str(self.NMAX)
    string=string+" "+str(self.NS)+" "+str(self.NS1)+" "+str(self.NS2)
    string=string+" "+str(self.IZZ)+" "+str(self.IOPT)+" "+str(self.HBRUIT)
    self.parent.txt.insert(INSERT,string)
    #canaux a ignorer
    self.insertTableOption(self.IZZ,1,self.IZ)
    #Options IO
    self.insertTableOption(self.IOPT,1,self.IO)
    #Plage de canaux Ã  lisser
    self.insertTableOption(self.IO[12],3,self.PLAGEL)
    #Plages de canaux a tracer
    self.insertTableOption(self.IO[16],1,self.GRASS)
    #Spectres
    nt=0
    while nt<self.NS:
      self.insertSpectre(nt)
      nt+=1
    print(self.sousSpectres[0].parameters)    
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
      
  def insertSpectre(self,nt):
    print("nt=",nt)
    string="\n"
    if self.sousSpectres[nt].kind==0:
      string+=str(self.sousSpectres[nt]["DI"])
      string+=" "
      string+=str(self.sousSpectres[nt]["GA"])
      string+=" "
      string+=str(self.sousSpectres[nt]["H1"])
      string+=" "
      string+=str(self.sousSpectres[nt]["SQ"])
      string+=" "
      string+=str(self.sousSpectres[nt]["CH"])
      string+=" "
      string+=str(self.sousSpectres[nt]["ETA"])
      string+=" "
      string+=str(self.sousSpectres[nt]["THETA"])
      string+=" "
      string+=str(self.sousSpectres[nt]["GAMMA"])
      string+=" "
      string+=str(self.sousSpectres[nt]["BETA"])
      string+=" "
      string+=str(self.sousSpectres[nt]["ALPHA"])
      string+=" "
      string+=str(self.sousSpectres[nt]["MONOC"])
      string+="\n"
      j=0
      while j<10:
        string=string+str(self.sousSpectres[nt].NB[j])+" "
        j+=1
      string+=str(self.sousSpectres[nt]["IOGV"])
      if self.sousSpectres[nt]["IOGV"]==3:
        string+="\n"
        j=0
        while j<8:
          string=string+str(self.sousSpectres[nt].GV[j])+" "
          j+=1
        string+="\n"
        j=0
        while j<8:
          string=string+str(self.sousSpectres[nt].NG[j])+" "
          j+=1
    elif self.sousSpectres[nt].kind==1:
      string+=str(self.sousSpectres[nt]["DI0"])
      string+=" "
      string+=str(self.sousSpectres[nt]["PDI"])
      string+=" "
      string+=str(self.sousSpectres[nt]["GA"])
      string+=" "
      string+=str(self.sousSpectres[nt]["H1"])
      string+=" "
      string+=str(self.sousSpectres[nt]["SQ0"])
      string+=" "
      string+=str(self.sousSpectres[nt]["PSQ"])
      string+=" "
      string+=str(self.sousSpectres[nt]["CH0"])
      string+=" "
      string+=str(self.sousSpectres[nt]["PCH"])
      string+=" "
      string+=str(self.sousSpectres[nt]["ETA0"])
      string+=" "
      string+=str(self.sousSpectres[nt]["THETA0"])
      string+=" "
      string+=str(self.sousSpectres[nt]["PTHETA"])
      string+=" "
      string+=str(self.sousSpectres[nt]["GAMMA0"])
      string+=" "
      string+=str(self.sousSpectres[nt]["BETA0"])
      string+=" "
      string+=str(self.sousSpectres[nt]["ALPHA0"])
      string+=" "
      string+=str(self.sousSpectres[nt]["MONOC0"])
      string+="\n"
      j=0
      while j<10:
        string=string+str(self.sousSpectres[nt].NB0[j])+" "
        j+=1
    else:
      string=''
    self.parent.txt.insert(INSERT,string)
  def setSpectres(self):
    # creation de la liste des sous-spectres
    self.sousSpectres=[]
    nt=0
    while nt<self.NS:
      if nt<self.NS1-1 :
        self.sousSpectres.append(Spectre(0))
      elif nt<self.NS2:
        self.sousSpectres.append(Spectre(nt-self.NS1+2))
      else:
        self.sousSpectres.append(Spectre(0))
      nt+=1
      
  def getTextLine(self):
    phrase=self.parent.txt.get("%d.%d" % (self.l, 0),"%d.end" % (self.l)).split()
    self.l+=1
    return phrase
    
  def textFromEntries(self):
    # Filling text space from  content of entries
    # Placement des valeurs des entrees dans les variables
    print("debut textFromEntries")
    print(self.sousSpectres[0].parameters)        
    self.variablesFromEntries()
    print("milieu textFromEntries")
    print(self.sousSpectres[0].parameters)
    # Remplissage du text à partir des variables
    self.textFromVariables()
    print("fin textFromEntries")
    print(self.sousSpectres[0].parameters)
    
  def variablesFromEntries(self):
    #lecture des chaines dans les champ d'entree et conversion en variables
    self.titre=self.parent.entries.etitre.get()
    self.CN=float(self.parent.entries.eCN.get())
    self.NMAX=int(self.parent.entries.eNMAX.get())
    self.NS= int(self.parent.entries.eNS.get())
    self.NS1=int(self.parent.entries.eNS1.get())
    self.NS2=int(self.parent.entries.eNS2.get())
    self.IZZ=int(self.parent.entries.eIZZ.get())
    self.IOPT=int(self.parent.entries.eIOPT.get())
    self.HBRUIT=float(self.parent.entries.eHBRUIT.get())
    if self.IZZ==1:
      i=0
      while i<10:
        self.IZ[i]= int(self.parent.entries.eIZ[i].get())
        i+=1
    if self.IOPT==1:
      i=0
      while i<20:
        self.IO[i]= int(self.parent.entries.eIO[i].get())
        i+=1
    if self.IO[12]==3:
      print("OK")
      self.PLAGEL[0]=int(self.parent.entries.ePLAGEL[0].get())
      self.PLAGEL[1]=int(self.parent.entries.ePLAGEL[1].get())
    if self.IO[16]!=0:
      i=0
      while i<10:
        self.GRASS[i]=int(self.parent.entries.eGRASS[i].get())
        i+=1
        
  def entriesFromText(self):
    self.variablesFromText()
    self.entriesFromVariables()
    
  def variablesFromText(self):
    #read data from text
    self.titre=self.parent.txt.get("0.0","1.end")
    self.l=2
    phrase=self.getTextLine()
    self.CN=float(phrase[0])
    self.NMAX= int(phrase[1])
    self.NS=  int(phrase[2])
    self.NS1= int(phrase[3])
    self.NS2= int(phrase[4])
    self.IZZ= int(phrase[5])
    self.IOPT= int(phrase[6])
    self.HBRUIT= float(phrase[7])
    if self.IZZ==1:
      phrase=self.getTextLine()
      i=0
      for mot in phrase:
        self.IZ[i]=int(mot)
        i=i+1
    if self.IOPT==1:
      phrase=self.getTextLine()
      i=0
      for mot in phrase:
        self.IO[i]=int(mot)
        i=i+1
    if self.IO[12]==3:
      phrase=self.getTextLine()
      if len(phrase)!=2:
        print("Mauvaise expression pour PLAGEL")
      self.PLAGEL[0]=int(phrase[0])
      self.PLAGEL[1]=int(phrase[1])
      print(self.PLAGEL)
    if self.IO[16]!=0:
      phrase=self.getTextLine()
      if len(phrase)!=10:
        print("Mauvaise expression pour GRASS")
      i=0
      for mot in phrase:
        self.GRASS[i]=int(mot)
        i+=1
      print(self.GRASS)
      # poursuivre avec la lecture des spectres
    print("len=",len(self.sousSpectres),"ns=",self.NS)
    self.setSpectres()
    print("len=",len(self.sousSpectres),"ns=",self.NS)
    nt=0
    while nt < self.NS:
      self.lireSpectre(nt)
      nt+=1

    
  def lireSpectre(self,nt):
    # lire le ntieme spectre sur la ligne l
    if (nt < self.NS1-1) or  (nt >= self.NS2) :
      phrase=self.getTextLine()
      print("nt=",nt)
      self.sousSpectres[nt].kind=0
      self.sousSpectres[nt]["DI"]=float(phrase[0])
      self.sousSpectres[nt]["GA"]=float(phrase[1])
      self.sousSpectres[nt]["H1"]=float(phrase[2])
      self.sousSpectres[nt]["SQ"]=float(phrase[3])
      self.sousSpectres[nt]["CH"]=float(phrase[4])
      self.sousSpectres[nt]["ETA"]=float(phrase[5])
      self.sousSpectres[nt]["THETA"]=float(phrase[6])
      self.sousSpectres[nt]["GAMMA"]=float(phrase[7])
      self.sousSpectres[nt]["ALPHA"]=float(phrase[8])
      self.sousSpectres[nt]["BETA"]=float(phrase[9])
      self.sousSpectres[nt]["MONOC"]=int(phrase[10])
      phrase=self.getTextLine()
      j=0
      while j<10:
        self.sousSpectres[nt].NB[j]=int(phrase[j])
        j+=1
      self.sousSpectres[nt]["IOGV"]= int(phrase[j])
      if self.sousSpectres[nt]["IOGV"]==3:
        phrase=self.getTextLine()
        j=0
        while j<8:
          self.sousSpectres[nt].GV[j]=int(phrase[j])
          j+=1
        phrase=self.getTextLine()
        while j<8:
          self.sousSpectres[nt].NG[j]=int(phrase[j])
          j+=1
    else:
      if nt==self.NS1-1:
        phrase=self.getTextLine()
        self.sousSpectres[nt].kind=1
        self.sousSpectres[nt]["DI0"]=float(phrase[0])
        self.sousSpectres[nt]["PDI"]=float(phrase[1])
        self.sousSpectres[nt]["GA"]=float(phrase[2])
        self.sousSpectres[nt]["H1"]=float(phrase[3])
        self.sousSpectres[nt]["SQ0"]=float(phrase[4])
        self.sousSpectres[nt]["PSQ"]=float(phrase[5])
        self.sousSpectres[nt]["CH0"]=float(phrase[6])
        self.sousSpectres[nt]["PCH"]=float(phrase[7])
        self.sousSpectres[nt]["ETA0"]=float(phrase[8])
        self.sousSpectres[nt]["PTHETA"]=float(phrase[9])
        self.sousSpectres[nt]["THETA0"]=float(phrase[10])
        self.sousSpectres[nt]["GAMMA0"]=float(phrase[11])
        self.sousSpectres[nt]["ALPHA0"]=float(phrase[12])
        self.sousSpectres[nt]["BETA0"]=float(phrase[13])
        self.sousSpectres[nt]["MONOC0"]=int(phrase[14])
        phrase=self.getTextLine()
        j=0
        while j<10:
          self.sousSpectres[nt].NB0[j]=int(phrase[j])
          j+=1
      else:
        self.sousSpectres[nt].kind=self.sousSpectres[nt-1].kind+1
        self.sousSpectres[nt]["DI0"]=self.sousSpectres[nt-1]["DI0"]
        self.sousSpectres[nt]["PDI"]=self.sousSpectres[nt-1]["PDI"]
        self.sousSpectres[nt]["GA"]=self.sousSpectres[nt-1]["GA"]
        self.sousSpectres[nt]["H1"]=self.sousSpectres[nt-1]["H1"]
        self.sousSpectres[nt]["SQ0"]=self.sousSpectres[nt-1]["SQ0"]
        self.sousSpectres[nt]["PSQ"]=self.sousSpectres[nt-1]["PSQ"]
        self.sousSpectres[nt]["CH0"]=self.sousSpectres[nt-1]["CH0"]
        self.sousSpectres[nt]["PCH"]=self.sousSpectres[nt-1]["PCH"]
        self.sousSpectres[nt]["ETA0"]=self.sousSpectres[nt-1]["ETA0"]
        self.sousSpectres[nt]["PTHETA"]=self.sousSpectres[nt-1]["PTHETA"]
        self.sousSpectres[nt]["THETA0"]=self.sousSpectres[nt-1]["THETA0"]
        self.sousSpectres[nt]["GAMMA0"]=self.sousSpectres[nt-1]["GAMMA0"]
        self.sousSpectres[nt]["ALPHA0"]=self.sousSpectres[nt-1]["ALPHA0"]
        self.sousSpectres[nt]["BETA0"]=self.sousSpectres[nt-1]["BETA0"]
        self.sousSpectres[nt]["MONOC0"]=self.sousSpectres[nt-1]["MONOC0"]
        self.sousSpectres[nt].NB0=self.sousSpectres[nt-1].NB0
      
  def entriesFromVariables(self):
    self.parent.entries.etitre.delete(0,END)
    self.parent.entries.etitre.insert(0,self.titre)
    self.parent.entries.eCN.delete(0, END)
    self.parent.entries.eCN.insert(0, str(self.CN))
    self.parent.entries.eNMAX.delete(0, END)
    self.parent.entries.eNMAX.insert(0, str(self.NMAX))
    self.parent.entries.eNS.delete(0, END)
    self.parent.entries.eNS.insert(0, str(self.NS))
    self.parent.entries.eNS1.delete(0, END)
    self.parent.entries.eNS1.insert(0, str(self.NS1))
    self.parent.entries.eNS2.delete(0, END)
    self.parent.entries.eNS2.insert(0, str(self.NS2))
    self.parent.entries.eIZZ.delete(0, END)
    self.parent.entries.eIZZ.insert(0, str(self.IZZ))
    self.parent.entries.eIOPT.delete(0, END)
    self.parent.entries.eIOPT.insert(0, str(self.IOPT))
    self.parent.entries.eHBRUIT.delete(0, END)
    self.parent.entries.eHBRUIT.insert(0, str(self.HBRUIT))
    if self.IZZ==1:
      i=0
      while i<10:
        self.parent.entries.eIZ[i].delete(0, END)
        self.parent.entries.eIZ[i].insert(0, str(self.IZ[i]))
        i+=1
    if self.IOPT==1:
      i=0
      while i<20:
        self.parent.entries.eIO[i].delete(0, END)
        self.parent.entries.eIO[i].insert(0, str(self.IO[i]))
        i+=1
    if self.IO[12]==3:
      for i in [0,1]:
        self.parent.entries.ePLAGEL[i].delete(0, END)
        self.parent.entries.ePLAGEL[i].insert(0, str(self.PLAGEL[i]))
    if self.IO[16]!=0:
      i=0
      while i<10:
        self.parent.entries.eGRASS[i].delete(0, END)
        self.parent.entries.eGRASS[i].insert(0, str(self.GRASS[i]))
        i+=1
        
#######################################################################
class Spectre():
  def __init__(self,kind):
    self.kind = kind #0 = sous-spectre standard, 1= premier sous spectre d'une distrib, 2 = 2eme, etc..
    self.parameters=dict()
    self["Titre"]="Sans Titre"
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
      self["IOGV"]=3
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
  fenetre.geometry("960x620+300+300")
  cadreEntree = Frame(fenetre)
  cadreBoutons = Frame(fenetre)
  cadreTexte = Frame(fenetre)
  p = PanedWindow(fenetre, orient=HORIZONTAL)
  p.add(cadreEntree)
  p.add(cadreBoutons)
  p.add(cadreTexte)
  fenetre.entries=DataEntries(cadreEntree,fenetre)
  barre = BarreMenu(fenetre)
  donnees = Data(fenetre,cadreTexte)
  
  p2 = PanedWindow(cadreBoutons, orient=VERTICAL)
  p2.pack(side=LEFT, expand=Y, fill=BOTH, pady=2, padx=2)
  b1 = Button(p2, text="interpreter .coo",command=donnees.entriesFromText)
  b2 = Button(p2, text="Convertir en .coo",command=donnees.textFromEntries)
  b1.pack(fill=Y)
  b2.pack(fill=Y)

  cadreEntree.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  cadreBoutons.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  cadreTexte.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  fenetre.txt.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  fenetre.entries.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  p.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  fenetre.mainloop()  
if __name__ == '__main__':
    main()  
