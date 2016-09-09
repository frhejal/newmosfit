#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
# Depending on version of Tkinter:
# Python 2             Python 3
# Tkinter         ->  tkinter
# Tix             ->  tkinter.tix
# ttk             ->  tkinter.ttk
# tkMessageBox    ->  tkinter.messagebox
# tkColorChooser  ->  tkinter.colorchooser
# tkFileDialog    ->  tkinter.filedialog
# tkCommonDialog  ->  tkinter.commondialog
# tkSimpleDialog  ->  tkinter.simpledialog
# tkFont          ->  tkinter.font
# Tkdnd           ->  tkinter.dnd
# ScrolledText    ->  tkinter.scrolledtext

#Python 2:
from Tkinter import *
import tkFileDialog

# Or Python 3 :

#~ from tkinter import *
#~ import tkinter.filedialog as tkFileDialog

########################################################################
class BarreMenu(Frame):
  #Top menu of the window
  def __init__(self, parent):
    Frame.__init__(self, parent)   #Barremenu is a frame
    self.parent = parent
    self.initUI() #truly create the menu

  def initUI(self):
    # Create File menu with Load, Save and Exit option.
    self.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2) #pack/set position
    menubar = Menu(self.parent)           # create menu
    self.parent.config(menu=menubar)
    #File menu      
    fileMenu = Menu(menubar)              
    fileMenu.add_command(label="Load", command=self.onOpen)
    fileMenu.add_command(label="Save as", command=self.onSave)
    fileMenu.add_separator()
    fileMenu.add_command(label="Exit", command=self.parent.destroy)        
    menubar.add_cascade(label="File", menu=fileMenu)        

  def onOpen(self):
    # Open dialog box to browse file system
    # Store content of .coo file in variable self.parent.txt
      ftypes = [('.coo', '*.coo'), ('All files', '*')]
      dlg = tkFileDialog.Open(self, filetypes = ftypes)
      fl = dlg.show()
      if fl==None:
        return
      if fl != '':
          text = self.readFile(fl)
          try:
            self.parent.txt.delete(0.0,END)
            self.parent.txt.insert(0.0, text)
          except Exception:
            pass
            
  def onSave(self):
    # Open dialog box to browse file system
    # Save content of variable self.parent.txt to .coo file
    ftypes = [('.coo', '*.coo'), ('All files', '*')]
    f = tkFileDialog.asksaveasfile(mode='w', defaultextension='*.coo',filetypes= ftypes)
    if f is None: # asksaveasfile return `None` if dialog closed with "cancel".
      return
    f.write(self.parent.txt.get(0.0,END))
    f.close()

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
    # General parameters
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
    for i in range(0,20):
      self.eIO.append(Entry(self,width=2,bg="light gray"))
    # Chanels to ignore
    self.eIZ=[]
    for i in range(0,10):
      self.eIZ.append(Entry(self,width=4,bg="light gray"))
    # Spectres to smooth
    self.ePLAGEL=[]
    for i in [0,1]:
      self.ePLAGEL.append(Entry(self,width=4,bg="light gray"))
    # Spectres to plot
    self.eGRASS=[]
    for i in range(0,10):
      self.eGRASS.append(Entry(self,width=4,bg="light gray"))
    
    #Create initial list of bogus spectres
    ns=4;
    ns1=2;
    ns2=3;
    self.spectres=[]
    self.spectres.append(Spectre(0))
    self.spectres.append(Spectre(1))
    self.spectres.append(Spectre(2))
    self.spectres.append(Spectre(0))

    # Change entries for spectres when NS, NS1 or NS2 entries are modified
    # uses StringVar objects for entries content
    self.strNS=StringVar()
    self.strNS1=StringVar()
    self.strNS2=StringVar()
    self.strNS.set(str(ns))
    self.strNS1.set(str(ns1))
    self.strNS2.set(str(ns2))
    self.strNS.trace("w",self.resetSpectres)
    self.strNS1.trace("w",self.resetSpectres)
    self.strNS2.trace("w",self.resetSpectres)
    self.eNS.config(validate=ALL,textvariable=self.strNS)
    self.eNS1.config(validate=ALL,textvariable=self.strNS1)
    self.eNS2.config(validate=ALL,textvariable=self.strNS2)

    # First creation of scrolling list and entries for spectres parameters
    self.setSpectresScroll(ns,ns1,ns2)
    self.packScrollbar()
    self.setEntrySpectre()
    self.packEntrySpectre()

    # Change fields colors according to selected options
    self.izz=StringVar()
    self.iopt=StringVar()
    self.izz.trace("w", self.checkIZZ)
    self.iopt.trace("w", self.checkIOPT)
    self.eIZZ.config(validate=ALL,textvariable=self.izz)
    self.eIOPT.config(validate=ALL,textvariable=self.iopt)
    
    # Change fields color in IO according to content
    self.strIO=[]
    for i in range(0,20):
      self.strIO.append(StringVar())
      if i not in [12, 16]:
        self.strIO[i].trace("w", self.checkIO)
        self.eIO[i].config(validate=ALL,textvariable=self.strIO[i])
    self.strIO[12].trace("w",self.checkIO13)
    self.strIO[16].trace("w",self.checkIO17)    
    self.eIO[12].config(validate=ALL,textvariable=self.strIO[12])
    self.eIO[16].config(validate=ALL,textvariable=self.strIO[16])
  def checkIZZ(self,*args):
    # Change background of entries IZ depending on entry IZZ
    try:
      izz=int(self.izz.get())
      if izz == 0:
        for i in range(0,10):
          self.eIZ[i].config(bg="light gray")
      elif izz == 1 :
        for i in range(0,10):
          self.eIZ[i].config(bg="white")
      else:
        raise ValueError
      self.eIZZ.config(bg="white")          
    except ValueError:
      self.eIZZ.config(bg="red")
      for i in range(0,10):
        self.eIZ[i].config(bg="light gray")
    except Exception as e:
      print(e)
      
  def checkIOPT(self,*args):
    # Change background color of entries IO depending on entry IOPT
    # Also calls checks on IO
    try:
      iopt=int(self.iopt.get())
      if iopt == 0:
        for i in range(0,20):
          self.eIO[i].config(bg="light gray")
      elif iopt == 1 :
        for i in range(0,20):
          self.eIO[i].config(bg="white")
      else:
        raise ValueError
      self.eIOPT.config(bg="white")
    except ValueError:
      # if incorrect value, redden the box.
      self.eIOPT.config(bg="red")
      for i in range(0,20):
        self.eIO[i].config(bg="light gray")
    self.checkIO17(args)
    self.checkIO(args)

  def checkIO(self,*args):
    # Check other entries for IO
    try:
      iopt=int(self.iopt.get())
    except ValueError:
      iopt=0
      
    if iopt!=1:
      #color in gray
      for i in range(0,20):
        self.eIO[i].config(bg="light gray")
    else:
      #color in white by defaut...
      for i in range(0,20):
        self.eIO[i].config(bg="white")
      # Then check if content is correct  
      try: 
        if int(self.strIO[0].get())<0 :
          raise ValueError
      except ValueError:
        self.eIO[0].config(bg="red")
      try:
        if int(self.strIO[1].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[1].config(bg="red")
      try:
        if int(self.strIO[2].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[2].config(bg="red")
      try:
        if int(self.strIO[3].get()) not in [0,1,2]: raise ValueError
      except ValueError :
        self.eIO[3].config(bg="red")
      try:
        if int(self.strIO[4].get())<0: raise ValueError
      except ValueError :
        self.eIO[4].config(bg="red")
      try:
        if int(self.strIO[5].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[5].config(bg="red")
      try:
        if int(self.strIO[6].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[6].config(bg="red")
      try:
        if int(self.strIO[7].get()) not in [0,1]: raise ValueError  
      except ValueError :
        self.eIO[7].config(bg="red")
      try:
        if int(self.strIO[8].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[8].config(bg="red")
      try:
        if int(self.strIO[9].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[9].config(bg="red")
      try:
        if int(self.strIO[10].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[10].config(bg="red")
      try:
        if int(self.strIO[11].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[11].config(bg="red")
      # Special function for IO(13) = self.eIO[12]:
      self.checkIO13(args)
      try:
        if int(self.strIO[13].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[13].config(bg="red")
      try:
        if int(self.strIO[14].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[14].config(bg="red")
      try:
        if int(self.strIO[15].get()) not in range(0,100): raise ValueError
      except ValueError :
        self.eIO[15].config(bg="red")
      # Special function for IO(17) = self.eIO[16] :
      self.checkIO17(args)        
      try:
        if int(self.strIO[17].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[17].config(bg="red")
      # io(19) not used.
      try:
        int(self.strIO[18].get())
        self.eIO[18].config(bg="light grey")
      except ValueError :
        self.eIO[18].config(bg="red")
      try:
        if int(self.strIO[19].get()) not in [0,1]: raise ValueError
      except ValueError :
        self.eIO[19].config(bg="red")
    
  def checkIO13(self,*args):
    # Change background color of entries PLAGEL depending on value of entry IO(13)
    # Also changes content of PLAGEL to match NS, and/or NS1 and NS2.
    try:
      io13=int(self.strIO[12].get())
      try:
        iopt=int(self.iopt.get())
      except ValueError:
        iopt=0
      if iopt==1:
        self.eIO[12].config(bg="white")
        self.ePLAGEL[0].config(bg="light gray")
        self.ePLAGEL[1].config(bg="light gray")
        if io13==0:
          self.ePLAGEL[0].delete(0,END)
          self.ePLAGEL[1].delete(0,END)
          self.ePLAGEL[0].insert(0,"0")
          self.ePLAGEL[1].insert(0,"0")
        elif io13==1:
          self.ePLAGEL[0].delete(0,END)
          self.ePLAGEL[1].delete(0,END)
          self.ePLAGEL[0].insert(0,"1")
          self.ePLAGEL[1].insert(0,self.strNS.get())
        elif io13==2:
          self.ePLAGEL[0].delete(0,END)
          self.ePLAGEL[1].delete(0,END)
          self.ePLAGEL[0].insert(0,self.strNS1.get())
          self.ePLAGEL[1].insert(0,self.strNS2.get())
        elif io13==3:
          self.ePLAGEL[0].config(bg="white")
          self.ePLAGEL[1].config(bg="white")
        else:
          raise ValueError
      else:
        self.ePLAGEL[0].config(bg="light gray")
        self.ePLAGEL[1].config(bg="light gray")
        self.eIO[12].config(bg="light gray")
    except ValueError:
      # if incorrect value, redden the box.
      self.eIO[12].config(bg="red")
      self.ePLAGEL[0].config(bg="light gray")
      self.ePLAGEL[1].config(bg="light gray")
    except Exception as e:
      print(e)
      
  def checkIO17(self,*args):
    # Changes background color of entries GRASS depending on value or entry IO(17)
    try:
      io17=int(self.strIO[16].get())
      try:
        iopt=int(self.iopt.get())
      except ValueError:
        iopt=0
      if iopt==1:
        self.eIO[16].config(bg="white")
        if io17==0:
          for i in range(0,10):
            self.eGRASS[i].config(bg="light gray")
        elif io17==1:
          for i in range(0,10):
            self.eGRASS[i].config(bg="white")
        else:
          raise ValueError
      else:  
        self.eIO[16].config(bg="light gray")
        for i in range(0,10):
          self.eGRASS[i].config(bg="light gray")
    except ValueError:
      # if incorrect value, redden the box.
      self.eIO[16].config(bg="red")
      for i in range(0,10):
        self.eGRASS[i].config(bg="light gray")
    except Exception as e:
      print(e)
      
  def checkIOGV(self,*args):
    # Change color of entries for NG, GV depending on value of IOGV.
    try:
      iogv=int(self.strIOGV.get())
      if iogv==1 or iogv==2:
        self.eNB[1].config(bg="light gray")
      else:
        self.eNB[1].config(bg="white")
      if iogv==3:
        for i in range(0,8):
          self.eGV[i].config(bg="white")
          self.eNG[i].config(bg="white")
      else:
        for i in range(0,8):
          self.eGV[i].config(bg="light gray")
          self.eNG[i].config(bg="light gray")
      if iogv not in [0, 1, 2, 3]:
        raise ValueError
      self.eIOGV.config(bg="white")
    except ValueError:
      self.eIOGV.config(bg="red")
    except Exception as e:
      print(e)
      pass
    self.saveSpectre(args)
    
  def resetSpectres(self,*args):
    #Check values of NS,NS1,NS2 entries, colorize entries.
    #Calls for resetting scroll list of spectres. 
   
    #read values :
    try:
      ns = int(self.strNS.get())
    except Exception:
      self.eNS.config(bg="red")
      ns=0
    try: 
      ns1 = int(self.strNS1.get())
    except Exception:
      ns1=0
    try:
      ns2 = int(self.strNS2.get())
    except Exception:
      ns2=0
    #Colorize in red if incompatible values, green if ok. 
    if ns1>ns:
      self.eNS.config(bg="light green")
      self.eNS1.config(bg="red")
      self.eNS2.config(bg="white")
    elif ns2>ns:
      self.eNS.config(bg="light green")
      self.eNS1.config(bg="white")
      self.eNS2.config(bg="red")
    elif ns1>ns2:
      self.eNS.config(bg="white")
      self.eNS1.config(bg="red")
      self.eNS2.config(bg="red")
    else:
      #reinitialize list of spectres
      self.delEntrySpectre()
      del self.spectres
      self.spectres=[]
      nt=1
      k=1
      #recreates list of spectres
      while nt<=ns :
        if ((nt < ns1) or (nt > ns2)) or (ns1<=0 or ns2<=0):
          self.spectres.append(Spectre(0))
        elif nt==ns1:
          self.spectres.append(Spectre(1))
          k+=1
        else:
          self.spectres.append(Spectre(k))
          k+=1
        nt+=1
      # recreates spectre scrol from ne list of spectres.
      self.setSpectresScroll( ns, ns1 , ns2 )
      self.packScrollbar()
      self.setEntrySpectre()
      self.packEntrySpectre()
      self.eNS.config(bg="white")
      self.eNS1.config(bg="white")
      self.eNS2.config(bg="white")
        
  def pack(self,**options):
    # Display all entries. 
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
    for i in range(0,20):
      Label(self, text="IO("+str(i+1)+")").grid(row=6+i ,sticky=E)
      self.eIO[i].grid(row=6+i,column=1,sticky=W)
    Label(self, text="Chanels to ignore:").grid(row=5, column=2, columnspan=3,sticky=W)
    for i in range(0,5):
      self.eIZ[2*i].grid(row=6+i,column=2,sticky=E)
      Label(self, text="to ").grid(row=6+i,column=3)
      self.eIZ[2*i+1].grid(row=6+i,column=4,sticky=W)
    Label(self, text="Spectres to plot:").grid(row=5, column=6, columnspan=4,sticky=W)
    for i in range(0,5):
      self.eGRASS[2*i].grid(row=6+i,column=6,sticky=E)
      Label(self, text="to ").grid(row=6+i,column=7)
      self.eGRASS[2*i+1].grid(row=6+i,column=8,sticky=W)
    Label(self, text="Spectres to smooth:").grid(row=11, column=2, columnspan=3,sticky=W)
    self.ePLAGEL[0].grid(row=12,column=2,sticky=E)
    Label(self, text="to ").grid(row=12, column=3)
    self.ePLAGEL[1].grid(row=12,column=4,sticky=W)
    Label(self, text="Spectres: ").grid(row=13,column=2)

  def packScrollbar(self):
    #Display scrollbar 
    self.scrollbar.grid(row=14,column=4,rowspan=5,sticky=W+N+S)
    self.listbox.grid(row=14,column=2,rowspan=5,columnspan=2)


  def setSpectresScroll(self,ns,ns1,ns2):
    #Create scollbar of spectres
    try:
      #destroy previous scollbar, if it exists.
      self.scrollbar.grid_forget()
      self.listbox.grid_forget()
    except Exception as e:
      print(e)
    #new scrollbar
    self.scrollbar = Scrollbar(self)
    self.listbox = Listbox(self,height= 5, width=10, selectmode=SINGLE, yscrollcommand=self.scrollbar.set)
    self.listbox.delete(0,END)
    #filling scrollbar with spectres (with number or "Distribution")
    self.lstSp=[]
    for nt in range(0,ns):
      if (ns1>0 and ns2>0):
        if (nt < ns1-1) or (nt >= ns2):
          self.listbox.insert(END,"Spectre "+str(nt+1))
          self.lstSp.append(nt)
        elif (nt==ns1-1):
          self.listbox.insert(END,"Distribution")
          self.lstSp.append(nt)
      else:
        self.listbox.insert(END,"Spectre "+str(nt+1))
        self.lstSp.append(nt)
    self.scrollbar.config(command=self.listbox.yview)
    self.listbox.bind('<<ListboxSelect>>',self.selectSpectre)
    self.selectedSpectre=0
    
  def selectSpectre(self,event):
    lb = event.widget
    self.selectedSpectre=self.lstSp[lb.curselection()[0]]
    self.delEntrySpectre()
    self.setEntrySpectre()
    self.packEntrySpectre()
    
  def setEntrySpectre(self):
    # Create entries.
    # Create StringVars and function to save all change in entries into current spectrum ,  self.spectres[self.selectedSpectre]
    s=self.spectres[self.selectedSpectre]

    if s.kind==0:
      #if new spectrum is not part of a distribution :
      self.strDI=StringVar()
      self.strGA=StringVar()
      self.strH1=StringVar()
      self.strSQ=StringVar()
      self.strCH=StringVar()
      self.strETA=StringVar()
      self.strTHETA=StringVar()
      self.strGAMMA=StringVar()
      self.strBETA=StringVar()
      self.strALPHA=StringVar()
      self.strMONOC=StringVar()
      self.strIOGV=StringVar()
      
      self.tDI=Label(self, text="DI")
      self.strDI.set(str(s.parameters["DI"])); self.strDI.trace("w",self.saveSpectre)
      self.eDI=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strDI);
      
      self.tGA=Label(self, text="GA")
      self.strGA.set(str(s.parameters["GA"])); self.strGA.trace("w",self.saveSpectre)
      self.eGA=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strGA);
      
      self.tH1=Label(self, text="H1")
      self.strH1.set(str(s.parameters["H1"])); self.strH1.trace("w",self.saveSpectre)
      self.eH1=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strH1);

      self.tSQ=Label(self, text=" SQ ")
      self.strSQ.set(str(s.parameters["SQ"])); self.strSQ.trace("w",self.saveSpectre)
      self.eSQ=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strSQ);
      
      self.tCH=Label(self, text=" CH ")
      self.strCH.set(str(s.parameters["CH"])); self.strCH.trace("w",self.saveSpectre)
      self.eCH=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strCH);
      
      self.tETA=Label(self, text="ETA")
      self.strETA.set(str(s.parameters["ETA"])); self.strETA.trace("w",self.saveSpectre)
      self.eETA=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strETA);
      
      self.tTHETA=Label(self, text=" THETA ")
      self.strTHETA.set(str(s.parameters["THETA"])); self.strTHETA.trace("w",self.saveSpectre)
      self.eTHETA=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strTHETA);
      
      self.tGAMMA=Label(self, text="GAMMA")
      self.strGAMMA.set(str(s.parameters["GAMMA"])); self.strGAMMA.trace("w",self.saveSpectre)
      self.eGAMMA=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strGAMMA);
      
      self.tBETA=Label(self, text="BETA")
      self.strBETA.set(str(s.parameters["BETA"])); self.strBETA.trace("w",self.saveSpectre)
      self.eBETA=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strBETA);
      
      self.tALPHA=Label(self, text="ALPHA")
      self.strALPHA.set(str(s.parameters["ALPHA"])); self.strALPHA.trace("w",self.saveSpectre)
      self.eALPHA=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strALPHA);
      
      self.tMONOC=Label(self, text=" MONOC ")
      self.strMONOC.set(str(s.parameters["MONOC"])); self.strMONOC.trace("w",self.saveSpectre)
      self.eMONOC=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strMONOC);

      self.tIOGV=Label(self, text="IOGV")
      self.strIOGV.set(str(s.parameters["IOGV"])); self.strIOGV.trace("w",self.checkIOGV)
      self.eIOGV=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strIOGV);
      # Array NB (adjustment parameters), 
      self.strNB=[]
      self.eNB=[]
      for j in range(0,10):
        self.strNB.append(StringVar())
        self.strNB[j].set(str(s.NB[j]))
        self.strNB[j].trace("w",self.saveSpectre)
        self.eNB.append(Entry(self,width=2,bg="white",validate=ALL,textvariable=self.strNB[j]))
      # Array GV and NG (use if IOGV==3)
      self.strGV=[]
      self.strNG=[]
      self.eGV=[]
      self.eNG=[]
      color="light gray"
      try:
        if int(str(self.strIOGV.get()))==3:color="white"
      except Exception as e :
        print(e)
      for j in range(0,8):
        self.strGV.append(StringVar())
        self.strNG.append(StringVar())
        self.strGV[j].set(str(s.GV[j]))
        self.strNG[j].set(str(s.NG[j]))
        self.strGV[j].trace("w",self.saveSpectre)
        self.strNG[j].trace("w",self.saveSpectre)
        self.eGV.append(Entry(self,width=5,bg=color,validate=ALL,textvariable=self.strGV[j]))
        self.eNG.append(Entry(self,width=2,bg=color,validate=ALL,textvariable=self.strNG[j]))
      # Legend for GV and NG
      self.tGV=Label(self,text="GV:")
      self.tNG=Label(self,text="NG:")
        
    else:
      #If part of a distribution :
      self.strDI0=StringVar()
      self.strPDI=StringVar()
      self.strGA0=StringVar()
      self.strH10=StringVar()
      self.strSQ0=StringVar()
      self.strPSQ=StringVar()
      self.strCH0=StringVar()
      self.strPCH=StringVar()
      self.strETA0=StringVar()
      self.strTHETA0=StringVar()
      self.strPTHETA=StringVar()
      self.strGAMMA0=StringVar()
      self.strBETA0=StringVar()
      self.strALPHA0=StringVar()
      self.strMONOC0=StringVar()
      
      self.tDI0=Label(self, text="DI0")
      self.strDI0.set(str(s.parameters["DI0"]));self.strDI0.trace("w",self.saveSpectre)
      self.eDI0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strDI0);

      self.strPDI.set(str(s.parameters["PDI"]));self.strPDI.trace("w",self.saveSpectre)
      self.ePDI=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strPDI);

      self.tGA0=Label(self, text="GA")
      self.strGA0.set(str(s.parameters["GA"]));self.strGA0.trace("w",self.saveSpectre)
      self.eGA0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strGA0);

      self.tH10=Label(self, text="H1")
      self.strH10.set(str(s.parameters["H1"]));self.strH10.trace("w",self.saveSpectre)
      self.eH10=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strH10);
      
      self.tSQ0=Label(self, text="SQ0")
      self.strSQ0.set(str(s.parameters["SQ0"]));self.strSQ0.trace("w",self.saveSpectre)
      self.eSQ0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strSQ0);    

      self.strPSQ.set(str(s.parameters["PSQ"]));self.strPSQ.trace("w",self.saveSpectre)
      self.ePSQ=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strPSQ);
            
      self.tCH0=Label(self, text="CH0")
      self.strCH0.set(str(s.parameters["CH0"]));self.strCH0.trace("w",self.saveSpectre)
      self.eCH0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strCH0);    

      self.strPCH.set(str(s.parameters["PCH"]));self.strPCH.trace("w",self.saveSpectre)
      self.ePCH=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strPCH);
            
      self.tETA0=Label(self, text="ETA")
      self.strETA0.set(str(s.parameters["ETA0"]));self.strETA0.trace("w",self.saveSpectre)
      self.eETA0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strETA0);   
      
      self.tTHETA0=Label(self, text="THETA0")
      self.strTHETA0.set(str(s.parameters["THETA0"]));self.strTHETA0.trace("w",self.saveSpectre)
      self.eTHETA0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strTHETA0);   
      
      self.strPTHETA.set(str(s.parameters["PTHETA"]));self.strPTHETA.trace("w",self.saveSpectre)
      self.ePTHETA=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strPTHETA);
      
      self.tGAMMA0=Label(self, text="GAMMA")
      self.strGAMMA0.set(str(s.parameters["GAMMA0"]));self.strGAMMA0.trace("w",self.saveSpectre)
      self.eGAMMA0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strGAMMA0);
      
      self.tBETA0=Label(self, text="BETA")
      self.strBETA0.set(str(s.parameters["BETA0"]));self.strBETA0.trace("w",self.saveSpectre)
      self.eBETA0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strBETA0);  
      
      self.tALPHA0=Label(self, text="ALPHA")
      self.strALPHA0.set(str(s.parameters["ALPHA0"]));self.strALPHA0.trace("w",self.saveSpectre)
      self.eALPHA0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strALPHA0);  
      
      self.tMONOC0=Label(self, text="MONOC0")
      self.strMONOC0.set(str(s.parameters["MONOC0"]));self.strMONOC0.trace("w",self.saveSpectre)
      self.eMONOC0=Entry(self,width=6,bg="white",validate=ALL,textvariable=self.strMONOC0); 
      
      self.strNB=[]
      self.eNB=[]
      for j in range(0,10):
        self.strNB.append(StringVar())
        self.strNB[j].set(str(s.NB0[j]))
        self.strNB[j].trace("w",self.saveSpectre)
        self.eNB.append(Entry(self,width=2,bg="white",validate=ALL,textvariable=self.strNB[j]))
      self.tStep=[]
      self.tStep.append(Label(self,text="Step:"))
      self.tStep.append(Label(self,text="Step:"))
    self.tParam=[]
    self.tParam.append(Label(self,text="Value:"))
    self.tParam.append(Label(self,text="Value:"))
    # Legend for NB
    self.tNB=[]
    self.tNB.append(Label(self,text="Adjust:"))
    self.tNB.append(Label(self,text="Adjust:"))


  def  saveSpectre(self,*args):
    #Save current values of entries for parameters (and associates) to spectre in list
    
    n=self.selectedSpectre
    if self.spectres[n].kind==0:
      try:
        self.spectres[n].parameters["DI"]=float(self.strDI.get())
        self.spectres[n].parameters["GA"]=float(self.strGA.get())
        self.spectres[n].parameters["H1"]=float(self.strH1.get())
        self.spectres[n].parameters["SQ"]=float(self.strSQ.get())
        self.spectres[n].parameters["CH"]=float(self.strCH.get())
        self.spectres[n].parameters["ETA"]=float(self.strETA.get())
        self.spectres[n].parameters["THETA"]=float(self.strTHETA.get())
        self.spectres[n].parameters["GAMMA"]=float(self.strGAMMA.get())
        self.spectres[n].parameters["BETA"]=float(self.strBETA.get())
        self.spectres[n].parameters["ALPHA"]=float(self.strALPHA.get())
        self.spectres[n].parameters["MONOC"]=int(self.strMONOC.get())
        self.spectres[n].parameters["IOGV"]=int(self.strIOGV.get())
        for j in range(0,10):
          self.spectres[n].NB[j]=self.strNB[j].get()
      except ValueError:
        pass
      try:
        if int(self.strIOGV.get())==3:
          for i in range(0,8):
            self.spectres[n].GV[i]=float(self.strGV[i].get())
            self.spectres[n].NG[i]=int(self.strNG[i].get())
      except ValueError:
        pass
        
    else:
      try:
        self.spectres[n].parameters["DI0"]=float(self.strDI0.get())
        self.spectres[n].parameters["PDI"]=float(self.strPDI.get())
        self.spectres[n].parameters["GA"]=float(self.strGA0.get())
        self.spectres[n].parameters["H1"]=float(self.strH10.get())
        self.spectres[n].parameters["SQ0"]=float(self.strSQ0.get())
        self.spectres[n].parameters["PSQ"]=float(self.strPSQ.get())
        self.spectres[n].parameters["CH0"]=float(self.strCH0.get())
        self.spectres[n].parameters["PCH"]=float(self.strPCH.get())
        self.spectres[n].parameters["ETA0"]=float(self.strETA0.get())
        self.spectres[n].parameters["THETA0"]=float(self.strTHETA0.get())
        self.spectres[n].parameters["PTHETA"]=float(self.strPTHETA.get())
        self.spectres[n].parameters["GAMMA0"]=float(self.strGAMMA0.get())
        self.spectres[n].parameters["BETA0"] =float(self.strBETA0.get())
        self.spectres[n].parameters["ALPHA0"]=float(self.strALPHA0.get())
        self.spectres[n].parameters["MONOC0"]=int(self.strMONOC0.get())
        for j in range(0,10):
          self.spectres[n].NB0[j]=self.strNB[j].get()
      except ValueError:
        pass

  def packEntrySpectre(self):
    s=self.spectres[self.selectedSpectre]
    r0=13
    c0=6
    r1=r0+4
    r2=r1+5
    c1=c0-2
    #Display parameters entries
    self.tParam[0].grid(row=r0+1,column=c0-1)
    self.tParam[1].grid(row=r1+1,column=c0-1)
    self.tNB[0].grid(row=r0+2,column=c0-1)
    self.tNB[1].grid(row=r1+2,column=c0-1)
    if s.kind==0:
      self.tDI.grid(row=r0,column=c0);self.eDI.grid(row=r0+1,column=c0)
      self.tGA.grid(row=r0,column=c0+1);self.eGA.grid(row=r0+1,column=c0+1)
      self.tH1.grid(row=r0,column=c0+2);self.eH1.grid(row=r0+1,column=c0+2)
      self.tSQ.grid(row=r0,column=c0+3);self.eSQ.grid(row=r0+1,column=c0+3)
      self.tCH.grid(row=r0,column=c0+4);self.eCH.grid(row=r0+1,column=c0+4)
      self.tETA.grid(row=r0,column=c0+5);self.eETA.grid(row=r0+1,column=c0+5)
      self.tTHETA.grid(row=r1,column=c0);self.eTHETA.grid(row=r1+1,column=c0)
      self.tGAMMA.grid(row=r1,column=c0+1);self.eGAMMA.grid(row=r1+1,column=c0+1)
      self.tBETA.grid(row=r1,column=c0+2);self.eBETA.grid(row=r1+1,column=c0+2)
      self.tALPHA.grid(row=r1,column=c0+3);self.eALPHA.grid(row=r1+1,column=c0+3)
      self.tMONOC.grid(row=r1,column=c0+5);self.eMONOC.grid(row=r1+1,column=c0+5)
      self.tIOGV.grid(row=r1+2,column=c0+5);self.eIOGV.grid(row=r1+3,column=c0+5)
      # display GV and NG
      self.tGV.grid(row=r2, column=c1-1)
      self.tNG.grid(row=r2+1, column=c1-1)
      for j in range(0,8):
        self.eGV[j].grid(row=r2,column=c1+j);
        self.eNG[j].grid(row=r2+1,column=c1+j);
    else:
      self.tDI0.grid(row=r0,column=c0);self.eDI0.grid(row=r0+1,column=c0)
      self.ePDI.grid(row=r0+3,column=c0)
      self.tGA0.grid(row=r0,column=c0+1);self.eGA0.grid(row=r0+1,column=c0+1)
      self.tH10.grid(row=r0,column=c0+2);self.eH10.grid(row=r0+1,column=c0+2)
      self.tSQ0.grid(row=r0,column=c0+3);self.eSQ0.grid(row=r0+1,column=c0+3)
      self.ePSQ.grid(row=r0+3,column=c0+3)
      self.tCH0.grid(row=r0,column=c0+4);self.eCH0.grid(row=r0+1,column=c0+4)
      self.ePCH.grid(row=r0+3,column=c0+4)
      self.tETA0.grid(row=r0,column=c0+5);self.eETA0.grid(row=r0+1,column=c0+5)
      self.tTHETA0.grid(row=r1,column=c0);self.eTHETA0.grid(row=r1+1,column=c0)
      self.ePTHETA.grid(row=r1+3,column=c0)
      self.tGAMMA0.grid(row=r1,column=c0+1);self.eGAMMA0.grid(row=r1+1,column=c0+1)
      self.tBETA0.grid(row=r1,column=c0+2);self.eBETA0.grid(row=r1+1,column=c0+2)
      self.tALPHA0.grid(row=r1,column=c0+3);self.eALPHA0.grid(row=r1+1,column=c0+3)
      self.tMONOC0.grid(row=r1,column=c0+5);self.eMONOC0.grid(row=r1+1,column=c0+5)
      self.tStep[0].grid(row=r0+3,column=c0-1)
      self.tStep[1].grid(row=r1+3,column=c0-1)
    # display NB entries
    for j in range(0,6):
      self.eNB[j].grid(row=r0+2,column=c0+j);
    for j in range(6,10):
      self.eNB[j].grid(row=r1+2,column=c0+j-6);

      
  def delEntrySpectre(self):
    try:
      self.eDI.grid_forget(); self.tDI.grid_forget()
      self.eGA.grid_forget(); self.tGA.grid_forget()
      self.eH1.grid_forget(); self.tH1.grid_forget()
      self.eSQ.grid_forget(); self.tSQ.grid_forget()
      self.eCH.grid_forget(); self.tCH.grid_forget()
      self.eETA.grid_forget(); self.tETA.grid_forget()
      self.eTHETA.grid_forget(); self.tTHETA.grid_forget()
      self.eGAMMA.grid_forget(); self.tGAMMA.grid_forget()
      self.eBETA.grid_forget(); self.tBETA.grid_forget()
      self.eALPHA.grid_forget(); self.tALPHA.grid_forget()
      self.eMONOC.grid_forget(); self.tMONOC.grid_forget()
      self.eIOGV.grid_forget(); self.tIOGV.grid_forget()
    except AttributeError:
      pass
    try:
      self.tDI0.grid_forget();self.eDI0.grid_forget()
      self.ePDI.grid_forget()
      self.tGA0.grid_forget();self.eGA0.grid_forget()
      self.tH10.grid_forget();self.eH10.grid_forget()
      self.tSQ0.grid_forget(); self.eSQ0.grid_forget()
      self.ePSQ.grid_forget()
      self.tCH0.grid_forget(); self.eCH0.grid_forget()
      self.ePCH.grid_forget()
      self.tTHETA0.grid_forget(); self.eTHETA0.grid_forget()
      self.ePTHETA.grid_forget()
      self.tGAMMA0.grid_forget(); self.eGAMMA0.grid_forget()
      self.tBETA0.grid_forget(); self.eBETA0.grid_forget()
      self.tALPHA0.grid_forget(); self.eALPHA0.grid_forget()
      self.tMONOC0.grid_forget(); self.eMONOC0.grid_forget()
      for i in [0,1]:
        self.tStep[i].grid_forget()
    except AttributeError:
      pass
    try:
      for i in [0,1]:
        self.tParam[i].grid_forget()
        self.tNB[i].grid_forget()
      for j in range(10):
        self.eNB[j].grid_forget()
    except AttributeError:
      pass
    try:
      self.tGV.grid_forget()
      self.tNG.grid_forget()
      for j in range(0,8):
        self.eGV[j].grid_forget()
        self.eNG[j].grid_forget()
    except AttributeError:
      pass
########################################################################
class Data():
  # Set of variables to store values between Text and Entries
  def __init__(self,parent,textmaster,):
    
    self.parent=parent
    self.parent.txt=Text(textmaster,bg="white")
    self.l=0 # latest line that was read in the text
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
    #Parameters of Spectres
    self.setSpectres()
    self.textFromVariables()
    
  def textFromVariables(self):
    # Transfer data form variables
    try:
      self.parent.txt.delete(0.0,END)
    except AttributeError:
      pass
    self.setText(self.titre+"\n")
    # main options
    string= str(self.CN)+" "+str(self.NMAX)
    string=string+" "+str(self.NS)+" "+str(self.NS1)+" "+str(self.NS2)
    string=string+" "+str(self.IZZ)+" "+str(self.IOPT)+" "+str(self.HBRUIT)
    self.parent.txt.insert(INSERT,string)
    # Ignored chanels
    self.insertTableOption(self.IZZ,1,self.IZ)
    # Options IO
    self.insertTableOption(self.IOPT,1,self.IO)
    # Chanels to be smoothed
    self.insertTableOption(self.IO[12],3,self.PLAGEL)
    # Chanels to be plotted
    self.insertTableOption(self.IO[16],1,self.GRASS)
    # Spectres
    for nt in range(0,self.NS):
      self.insertSpectre(nt)
    # rewrite the rest (experimental spectre and/or spectre of noise)
    try:
      self.parent.txt.insert(INSERT,"\n"+self.expdata)
    except AttributeError:
      pass
    
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
      for j in range(0,10):
        string=string+str(self.sousSpectres[nt].NB[j])+" "
      string+=str(self.sousSpectres[nt]["IOGV"])
      if self.sousSpectres[nt]["IOGV"]==3:
        string+="\n"
        for j in range(0,8):
          string=string+str(self.sousSpectres[nt].GV[j])+" "
        string+="\n"
        for j in range(0,8):
          string=string+str(self.sousSpectres[nt].NG[j])+" "
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
      for j in range(0,10):
        string=string+str(self.sousSpectres[nt].NB0[j])+" "
    else:
      string=''
    self.parent.txt.insert(INSERT,string)
    
  def setSpectres(self):
    # Create list of spectres
    self.sousSpectres=[]
    for nt in range(self.NS):
      if nt<self.NS1-1 :
        self.sousSpectres.append(Spectre(0))
      elif nt<self.NS2 and self.NS2>0 and self.NS1>0:
        self.sousSpectres.append(Spectre(nt-self.NS1+2))
      else:
        self.sousSpectres.append(Spectre(0))
      
  def getTextLine(self):
    phrase=self.parent.txt.get("%d.%d" % (self.l, 0),"%d.end" % (self.l)).split()
    self.l+=1
    return phrase
  def textFromEntries(self):
    # Filling text space from  content of entries
    self.variablesFromEntries()
    self.textFromVariables()
    
  def variablesFromEntries(self):
    # Read strings from entries and convert to variables
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
      for i in range(0,10):
        self.IZ[i]= int(self.parent.entries.eIZ[i].get())
    if self.IOPT==1:
      for i in range(0,20):
        self.IO[i]= int(self.parent.entries.eIO[i].get())
    if self.IO[12]==3:
      self.PLAGEL[0]=int(self.parent.entries.ePLAGEL[0].get())
      self.PLAGEL[1]=int(self.parent.entries.ePLAGEL[1].get())
    if self.IO[16]!=0:
      for i in range(0,10):
        self.GRASS[i]=int(self.parent.entries.eGRASS[i].get())
    # Spectres
    self.lireSpectresFromEntries()
      
  def lireSpectresFromEntries(self):
    # Copy list of spectres contains in entres
    try:
      del self.sousSpectres
    except AttributeError:
      pass
    self.sousSpectres=[]
    for nt in range(0,self.NS):
      if nt<self.NS1 :
        self.sousSpectres.append(self.parent.entries.spectres[nt])
      elif nt<self.NS2 and self.NS2>0:
        self.sousSpectres.append(Spectre(nt-self.NS1+2))
      else:
        self.sousSpectres.append(self.parent.entries.spectres[nt])
      
  def entriesFromText(self):
    # Fill entries with data read from text (through storage in Data)
    self.variablesFromText()
    self.entriesFromVariables()


  def variablesFromText(self):
    # Read data from text
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
    # Read spectres
    self.setSpectres()
    for nt in range(0,self.NS):
      self.lireSpectre(nt)
    # Save the rest of data ( noise spectre and/or experimental data)
    self.expdata = self.parent.txt.get("%d.%d" % (self.l, 0),END )
    
  def lireSpectre(self,nt):
    # read NTth spectre on line l
    if ((nt < self.NS1-1) or (nt >= self.NS2)) or (self.NS1<=0 or self.NS2<=0) :
      # If not a spectre from the distribution
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
      for j in range(0,10):
        self.sousSpectres[nt].NB[j]=int(phrase[j])
      self.sousSpectres[nt]["IOGV"]= int(phrase[10])
      if self.sousSpectres[nt]["IOGV"]==3:
        phrase=self.getTextLine()
        for j in range(0,8):
          self.sousSpectres[nt].GV[j]=float(phrase[j])
        phrase=self.getTextLine()
        for j in range(0,8):
          self.sousSpectres[nt].NG[j]=int(phrase[j])
    elif nt==self.NS1-1:
      # If first spectre of the distribution
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
      for j in range(0,10):
        self.sousSpectres[nt].NB0[j]=int(phrase[j])
    else:
      # If any other spectre of the distribution
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
    # Fill with values stored in Data. 
    # Global options
    self.parent.entries.eTitre.delete(0,END)
    self.parent.entries.eTitre.insert(0,self.titre)
    self.parent.entries.eCN.delete(0, END)
    self.parent.entries.eCN.insert(0, str(self.CN))
    self.parent.entries.eNMAX.delete(0, END)
    self.parent.entries.eNMAX.insert(0, str(self.NMAX))

    self.parent.entries.strNS.set(str(self.NS))
    self.parent.entries.strNS1.set(str(self.NS1))
    self.parent.entries.strNS2.set(str(self.NS2))
    
    self.parent.entries.izz.set(str(self.IZZ))
    self.parent.entries.iopt.set(str(self.IOPT))
    self.parent.entries.eHBRUIT.delete(0, END)
    self.parent.entries.eHBRUIT.insert(0, str(self.HBRUIT))
    # Entries for IZ
    if self.IZZ==1:
      for i in range(0,10):
        self.parent.entries.eIZ[i].delete(0, END)
        self.parent.entries.eIZ[i].insert(0, str(self.IZ[i]))
    # Entries for IO
    if self.IOPT==1:
      for i in range(0,20):
        self.parent.entries.strIO[i].set(str(self.IO[i]))
    # Entries for PLAGEL
    if self.IO[12]==3:
      for i in [0,1]:
        self.parent.entries.ePLAGEL[i].delete(0, END)
        self.parent.entries.ePLAGEL[i].insert(0, str(self.PLAGEL[i]))
    # Entries for GRASS
    if self.IO[16]!=0:
      for i in range(0,10):
        self.parent.entries.eGRASS[i].delete(0, END)
        self.parent.entries.eGRASS[i].insert(0, str(self.GRASS[i]))
    # Entries for parameters of spectres
    self.parent.entries.spectres=list(self.sousSpectres)
    self.parent.entries.setSpectresScroll(self.NS,self.NS1,self.NS2)
    self.parent.entries.delEntrySpectre()
    self.parent.entries.packScrollbar()
    self.parent.entries.setEntrySpectre()
    self.parent.entries.packEntrySpectre()
#######################################################################
class Spectre():
  def __init__(self,kind):
    # Create a new spectre with default value
    self.kind = kind #0 = standard spectre, 1= 1rst spectre of a distribution, 2 = 2nd, etc..
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
########################################################################
def main():
  # Create window
  fenetre = Tk()
  fenetre.title("Mosfit2016")
  fenetre.geometry("1280x620+300+300")
  # Creates 3 frames
  cadreEntree = Frame(fenetre)
  cadreBoutons = Frame(fenetre)
  cadreTexte = Frame(fenetre)
  #create paned windows for the 3 frames
  p = PanedWindow(fenetre, orient=HORIZONTAL)
  p.add(cadreEntree)
  p.add(cadreBoutons)
  p.add(cadreTexte)
  # Menu
  barre = BarreMenu(fenetre)
  # Entries for manual input
  fenetre.entries=DataEntries(cadreEntree,fenetre)
  # Data storage space
  fenetre.donnees = Data(fenetre,cadreTexte)
  # Buttons to convert entries to text and vice versa
  p2 = PanedWindow(cadreBoutons, orient=VERTICAL)
  p2.pack(side=LEFT, expand=Y, fill=BOTH, pady=2, padx=2)
  b1 = Button(p2, text="Decypher .coo",command=fenetre.donnees.entriesFromText)
  b2 = Button(p2, text="Translate to .coo",command=fenetre.donnees.textFromEntries)
  b1.pack(fill=Y)
  b2.pack(fill=Y)
  # plot all
  cadreEntree.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  cadreBoutons.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  cadreTexte.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  fenetre.txt.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  fenetre.entries.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  p.pack(side=LEFT,expand=Y, fill=BOTH, pady=2, padx=2)
  # Main loop
  fenetre.mainloop()  
if __name__ == '__main__':
    main()  
