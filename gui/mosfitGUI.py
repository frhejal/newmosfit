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
    # Create File menu with Load an Exit option.
    self.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)

    menubar = Menu(self.parent)
    self.parent.config(menu=menubar)

    fileMenu = Menu(menubar)
    fileMenu.add_command(label="Load", command=self.onOpen)
    fileMenu.add_separator()
    fileMenu.add_command(label="Exit", command=self.parent.quit)        
    menubar.add_cascade(label="File", menu=fileMenu)        

  def onOpen(self):
    # Open dialog box to browse file system
    # Store content of .coo file in variable self.parent.txt
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
    # open and read .coo file
    f = open(filename, "r")
    text = f.read()
    return text
########################################################################
class DataEntries(PanedWindow):
  # Panel containing all entry fields for options
  def __init__(self,master,parent):
    
    PanedWindow.__init__(self,master)
    self.parent=parent

    self.eTitre=Entry(self,width=30,bg="white")
    self.eCN=Entry(self,width=10,bg="white")
    self.eNMAX=Entry(self,width=4,bg="white")
    self.eNS=Entry(self,width=2,bg="white")
    self.eNS1=Entry(self,width=2,bg="white")
    self.eNS2=Entry(self,width=2,bg="white")
    self.eIZZ=Entry(self,width=2,bg="white")
    self.eIOPT=Entry(self,width=2,bg="white")
    self.eHBRUIT=Entry(self,width=8,bg="white")
    # Options
    self.eIO =[]
    i=0
    while i<20:
      self.eIO.append(Entry(self,width=2,bg="light gray"))
      i+=1
    # Chanels to ignore
    self.eIZ=[]
    i=0
    while i<10:
      self.eIZ.append(Entry(self,width=4,bg="light gray"))
      i+=1
    # Spectres to smooth
    self.ePLAGEL=[]
    for i in [0,1]:
      self.ePLAGEL.append(Entry(self,width=4,bg="light gray"))
    # Spectres to plot
    i=0
    self.eGRASS=[]
    while i<10:
      self.eGRASS.append(Entry(self,width=4,bg="light gray"))
      i+=1
    #Create initial list of bogus spectres
    ns=4;self.eNS.insert(0,str(ns))
    ns1=2;self.eNS1.insert(0,str(ns1))
    ns2=3;self.eNS2.insert(0,str(ns2))

    self.spectres=[]
    self.spectres.insert(0,Spectre(0))
    self.spectres.insert(1,Spectre(1))
    self.spectres.insert(2,Spectre(2))
    self.spectres.insert(3,Spectre(0))
    self.setSpectresScroll(ns,ns1,ns2)
    self.setEntrySpectre()
    
    # Change fields colors according to selected options
    self.izz=StringVar()
    self.iopt=StringVar()
    self.io13=StringVar()
    self.io17=StringVar()
  
    self.izz.trace("w", self.checkIZZ)
    self.iopt.trace("w", self.checkIOPT)
    self.io13.trace("w",self.checkIO13)
    self.io17.trace("w",self.checkIO17)
    self.eIZZ.config(validate=ALL,textvariable=self.izz)
    self.eIOPT.config(validate=ALL,textvariable=self.iopt)
    self.eIO[12].config(validate=ALL,textvariable=self.io13)
    self.eIO[16].config(validate=ALL,textvariable=self.io17)
    # Change entries for spectres when NS, NS1 or NS2 entries are modified
    
    
    #~ self.ns=StringVar()
    #~ self.ns1=StringVar()
    #~ self.ns2=StringVar()
    #~ self.ns.trace("w",self.setSpectresEntries)
    #~ self.ns1.trace("w",self.setSpectresEntries)
    #~ self.ns2.trace("w",self.setSpectresEntries)
    #~ self.eNS.config(validate=ALL,textvariable=self.ns)
    #~ self.eNS1.config(validate=ALL,textvariable=self.ns1)
    #~ self.eNS2.config(validate=ALL,textvariable=self.ns2)
    
    #~ self.spectresEntries = Frame(self)
    #~ self.textSpectre=Text(self.spectresEntries,height=10, bg="white")
    #~ self.scrollbar = Scrollbar(self)

    #~ spectresScroll = Frame(self,height=10,width=30, yscrollcommand=self.scrollbar.set)
    #~ for i in range(40):
        #~ listbox.insert(END, str(i+1))
    #~ listbox.grid(row=14,column=2,columnspan=7,rowspan=4)

    #~ self.scrollbar.config(command=listbox.yview)

  #~ def set(self):
    #~ print("view modified")
  #~ def yview(self):
    #~ print("manipulation")

      
  #~ def setSpectresEntries(self):
    #~ # To be recalculated every time NS, NS1 or NS2 entry are changed
    #~ try:
      #~ ns=int(self.ns.get())
      #~ ns1=int(self.ns1.get())
      #~ ns2=int(self.ns2.get())
    #~ except Exception:
      #~ ns=0
      #~ ns1=0
      #~ ns2=0
    #~ nt=0
    #~ k=0
    #~ self.Spectres=[]
    #~ while(nt<ns):
      #~ if nt < ns1 or nt> ns2:
        #~ self.Spectres[k].add(EntrySpectre(self,parent.donnees.sousSpectres[nt]) )
        #~ k=k+1
    #~ nt+=1
    
    
    # Afficher entrees
    
  def checkIZZ(self,*args):
    try:
        if int(self.izz.get())!=1:
          i=0
          while i<10:
            self.eIZ[i].config(bg="light gray")
            i+=1
        else:
          i=0
          while i<10:
            self.eIZ[i].config(bg="white")
            i+=1
    except:
      i=0
      while i<10:
        self.eIZ[i].config(bg="light gray")
        i+=1
        
  def checkIOPT(self,*args):
    try:
        if int(self.iopt.get())!=1:
          i=0
          while i<20:
            self.eIO[i].config(bg="light gray")
            i+=1
        else:
          i=0
          while i<20:
            self.eIO[i].config(bg="white")
            i+=1
    except:
          i=0
          while i<20:
            self.eIO[i].config(bg="light gray")
            i+=1
    self.checkIO13()
    self.checkIO17()
    
  def checkIO13(self,*args):
    try:
        if int(self.io13.get())==3   and int(self.iopt.get())==1:
          self.ePLAGEL[0].config(bg="white")
          self.ePLAGEL[1].config(bg="white")
        else:
          self.ePLAGEL[0].config(bg="light gray")
          self.ePLAGEL[1].config(bg="light gray")
    except:
      self.ePLAGEL[0].config(bg="light gray")
      self.ePLAGEL[1].config(bg="light gray")
      
  def checkIO17(self,*args):
    try:
      if int(self.io17.get())==1 and int(self.iopt.get())==1:
        i=0
        while i<10:
          self.eGRASS[i].config(bg="white")
          i+=1
      else:
        i=0
        while i<10:
          self.eGRASS[i].config(bg="light gray")
          i+=1
    except:
      i=0
      while i<10:
        self.eGRASS[i].config(bg="light gray")
        i+=1

  def pack(self,**options):
    # affichages
    PanedWindow.grid(self,row=0)
    Label(self, text="Title").grid(row=0);self.eTitre.grid(row=0,column=1,columnspan=5,sticky=W)
    Label(self, text="CN").grid(row=1);self.eCN.grid(row=1,column=1,columnspan=3,sticky=W)
    Label(self, text="NMAX").grid(row=1,column=4);self.eNMAX.grid(row=1,column=5,sticky=W)
    Label(self, text="NS").grid(row=2);self.eNS.grid(row=2,column=1,sticky=W)
    Label(self, text="NS1").grid(row=2,column=2);self.eNS1.grid(row=2,column=3,sticky=W)
    Label(self, text="NS2").grid(row=2,column=4);self.eNS2.grid(row=2,column=5,sticky=W)
    Label(self, text="IZZ").grid(row=3);self.eIZZ.grid(row=3,column=1,sticky=W)
    Label(self, text="IOPT").grid(row=3,column=2);self.eIOPT.grid(row=3,column=3,sticky=W)
    Label(self, text="HBRUIT").grid(row=4);self.eHBRUIT.grid(row=4,column=1,columnspan=2,sticky=W)
    Label(self, text="Options").grid(row=5,columnspan=2)
    i=0
    while i<20:
      Label(self, text="IO("+str(i+1)+")").grid(row=6+i ,sticky=E)
      self.eIO[i].grid(row=6+i,column=1,sticky=W)
      i+=1
    Label(self, text="Chanels to ignore:").grid(row=5, column=2, columnspan=3,sticky=W)
    i=0
    while i<5:
      self.eIZ[2*i].grid(row=6+i,column=2,sticky=E)
      Label(self, text="to ").grid(row=6+i,column=3)
      self.eIZ[2*i+1].grid(row=6+i,column=4,sticky=W)
      i+=1
    Label(self, text="Spectres to plot:").grid(row=5, column=6, columnspan=4,sticky=W)
    i=0
    while i<5:
      self.eGRASS[2*i].grid(row=6+i,column=6,sticky=E)
      Label(self, text="to ").grid(row=6+i,column=7)
      self.eGRASS[2*i+1].grid(row=6+i,column=8,sticky=W)
      i+=1
    Label(self, text="Spectres to smooth:").grid(row=11, column=2, columnspan=3,sticky=W)
    self.ePLAGEL[0].grid(row=12,column=2,sticky=E)
    Label(self, text="to ").grid(row=12, column=3)
    self.ePLAGEL[1].grid(row=12,column=4,sticky=W)
    Label(self, text="Spectres: ").grid(row=13,column=2)

  def packScrollbar(self):
    self.scrollbar.grid(row=13,column=4,rowspan=5)
    self.scrollbar.grid(row=13,column=4,rowspan=5)
    self.listbox.grid(row=14,column=2,rowspan=5,columnspan=2)
    self.packEntrySpectre()

  def setSpectresScroll(self,ns,ns1,ns2):
    # set scroll bar to allow user to choose spectre
    try:
      self.scrollbar.grid_forget()
      self.listbox.grid_forget()
      del self.scrollbar
      del self.listbox
    except:
      pass
    self.scrollbar = Scrollbar(self)
    self.listbox = Listbox(self,height= 5, width=10, selectmode=SINGLE, yscrollcommand=self.scrollbar.set)
    self.listbox.delete(0,END)
    nt=0
    k=0
    self.lstSp=[]
    while nt<ns :
      if((nt < ns1-1) or (nt >= ns2)) or (ns1<=0 or ns2<=0) :
        print("nt",nt, ns1-1, ns2)
        self.listbox.insert(END,"Spectre "+str(nt+1))
        self.lstSp.insert(k+1,nt)
        k+=1
      elif nt == ns1-1:
        print("nt dist",nt, ns1-1, ns2)
        self.listbox.insert(k+1,"Distribution")
        self.lstSp.insert(k,nt)
        k+=1
      nt+=1
    print(self.lstSp)
    self.scrollbar.config(command=self.listbox.yview)
    self.listbox.bind('<<ListboxSelect>>',self.selectSpectre)
    self.selectedSpectre=0
  def selectSpectre(self,event):
    lb = event.widget
    self.selectedSpectre=self.lstSp[lb.curselection()[0]]
    self.setEntrySpectre()
    self.packEntrySpectre()
    
  def setEntrySpectre(self):
    # creer entrees
    s=self.spectres[self.selectedSpectre]
    if s.kind==0:
      self.tDI=Label(self, text="DI")
      self.eDI=Entry(self,width=6,bg="white"); self.eDI.insert(0,str(s.parameters["DI"]))
      self.tGA=Label(self, text="GA")
      self.eGA=Entry(self,width=6,bg="white"); self.eGA.insert(0,str(s.parameters["GA"]))
    else:
      self.tDI0=Label(self, text="DI0")
      self.eDI0=Entry(self,width=6,bg="white"); self.eDI0.insert(0,str(s.parameters["DI0"]))
      self.tPDI=Label(self, text="PDI")
      self.ePDI=Entry(self,width=6,bg="white"); self.ePDI.insert(0,str(s.parameters["PDI"]))
      self.tGA0=Label(self, text="GA")
      self.eGA0=Entry(self,width=6,bg="white"); self.eGA0.insert(0,str(s.parameters["GA"]))        
  def packEntrySpectre(self):
    s=self.spectres[self.selectedSpectre]
    #effacement anciennes entrees
    try:
      self.eDI.grid_forget(); self.tDI.grid_forget()
      self.eGA.grid_forget(); self.tGA.grid_forget()
    except:
      pass
    try:
      self.tDI0.grid_forget();self.eDI0.grid_forget()
      self.tPDI.grid_forget();self.ePDI.grid_forget()
      self.tGA.grid_forget();self.eGA.grid_forget()
    except:
      pass        
    if s.kind==0:
      self.tDI.grid(row=19,column=2);self.eDI.grid(row=20,column=2)
      self.tGA.grid(row=19,column=3);self.eGA.grid(row=20,column=3)
    else:
      self.tDI0.grid(row=19,column=2);self.eDI0.grid(row=20,column=2)
      self.tPDI.grid(row=21,column=2);self.ePDI.grid(row=22,column=2)
      self.tGA0.grid(row=19,column=3);self.eGA0.grid(row=20,column=3)
########################################################################
#~ class EntrySpectre(Frame, ):
  #~ #Contains entries for spectres
  #~ def __init__(self,master,spectre):
    #~ #premiere creation de champ spectre : utilisation des donnes par defaut des variables
    #~ Frame.__init__(self,master)
    #~ if spectre.kind == 0 :
      #~ self.eDI=Entry(self,width=30,bg="white")
    #~ else :
      #~ self.DI0=Entry(self,width=30,bg="white")
  #~ def pack(self,**options):
    #~ pass
########################################################################
class Data():
  # Set of variables to store values between Text and Entries
  def __init__(self,parent,textmaster,):
    
    self.parent=parent
    self.parent.txt=Text(textmaster,bg="white")
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
      elif nt<self.NS2 and self.NS2>0:
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
    self.variablesFromEntries()
    # Remplissage du text à partir des variables
    self.textFromVariables()
    
  def variablesFromEntries(self):
    #lecture des chaines dans les champ d'entree et conversion en variables
    self.titre=self.parent.entries.eTitre.get()
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
    self.parent.entries.spectres=self.sousSpectres
    self.parent.entries.setSpectresScroll(self.NS,self.NS1,self.NS2)
    self.parent.entries.packScrollbar()
    self.parent.entries.setEntrySpectre()
    self.parent.entries.packEntrySpectre()
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
    if self.IO[16]!=0:
      phrase=self.getTextLine()
      if len(phrase)!=10:
        print("Mauvaise expression pour GRASS")
      i=0
      for mot in phrase:
        self.GRASS[i]=int(mot)
        i+=1
      # poursuivre avec la lecture des spectres
    self.setSpectres()
    nt=0
    while nt < self.NS:
      self.lireSpectre(nt)
      nt+=1

    
  def lireSpectre(self,nt):
    # lire le ntieme spectre sur la ligne l
    if ((nt < self.NS1-1) or (nt >= self.NS2)) or (self.NS1<=0 or self.NS2<=0) :
      phrase=self.getTextLine()
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
    self.parent.entries.eTitre.delete(0,END)
    self.parent.entries.eTitre.insert(0,self.titre)
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
    #ajouter remplacement des variables des entrees de spectre:
        
#######################################################################
class Spectre():
  def __init__(self,kind):
    self.kind = kind #0 = sous-spectre standard, 1= premier sous spectre d'une distrib, 2 = 2eme, etc..
    self.parameters=dict()
    self["Titre"]="Sans Titre"
    if self.kind==0:
      self["DI"]=0.0
      self["GA"]=0.1
      self["H1"]=0.0
      self["SQ"]=0.0
      self["CH"]=0.0
      self["ETA"]=0.0
      self["THETA"]=0.0
      self["GAMMA"]=0.0
      self["ALPHA"]=0.0
      self["BETA"]=0.0
      self["MONOC"]=0
      self["IOGV"]=3
      self.NB=[0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 
      self.GV=[0, 0, 0, 0, 0, 0, 0, 0] 
      self.NG=[0, 0, 0, 0, 0, 0, 0, 0] 
    else:
      self["DI0"]=0.0
      self["PDI"]=0.0
      self["GA"]=0.1
      self["H1"]=0.0
      self["SQ0"]=0.0
      self["PSQ"]=0.0
      self["CH0"]=0.0
      self["PCH"]=0.0
      self["ETA0"]=0.0
      self["PTHETA"]=0.0
      self["THETA0"]=0.0
      self["GAMMA0"]=0.0
      self["ALPHA0"]=0.0
      self["BETA0"]=0.0
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
  fenetre.geometry("1060x620+300+300")
  cadreEntree = Frame(fenetre)
  cadreBoutons = Frame(fenetre)
  cadreTexte = Frame(fenetre)
  p = PanedWindow(fenetre, orient=HORIZONTAL)
  p.add(cadreEntree)
  p.add(cadreBoutons)
  p.add(cadreTexte)
  fenetre.entries=DataEntries(cadreEntree,fenetre)
  barre = BarreMenu(fenetre)
  fenetre.donnees = Data(fenetre,cadreTexte)
  
  p2 = PanedWindow(cadreBoutons, orient=VERTICAL)
  p2.pack(side=LEFT, expand=Y, fill=BOTH, pady=2, padx=2)
  b1 = Button(p2, text="Decypher .coo",command=fenetre.donnees.entriesFromText)
  b2 = Button(p2, text="Translate to .coo",command=fenetre.donnees.textFromEntries)
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
