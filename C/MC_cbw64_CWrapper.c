/*------------------------------------------------------------------------------------------
Wrapper pour la librarie cbw64.dll de Measurement Computing. Rend quelques fonctions de  cette librairie compatible avec l'interface .C de R. Essentiellement, on s'assure que les fonctions du wrapper retourne un void et on gère à l'intérieur des fonctions le formattage approprié des paramètres à passer.
Le fichier d'aide de Measurement Computing comprend les infos nécessaires (prototypes des fonctions). On peut aussi consulter le fichier cbw.h pour différentes définitions de constantes.
On compile la fonction à l'aide du script compile_script.R. 
Attention à ajuster le chemin des répertoires selon l'installation. Quelques "warnings" en compilant mais ça marche bien.
--------------------------------------------------------------------------------------------
Auteur: Bernard Panneton, Agriculture et Agroalimentaire Canada, St-Jean-sur-Richelieu
Janvier 2017.
--------------------------------------------------------------------------------------------
*/

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <stdio.h>
#include <windows.h>


typedef enum DaqDeviceInterface DaqDeviceInterface;
enum DaqDeviceInterface
{
  Any,
  Bluetooth,
  Ethernet,
  USB
}; // don't forget the semi-colon ; 


struct  DDDesc 
{   
  char ProductName[64]; 
  unsigned int ProductID; 
  int InterfaceType; 
  char DevString[64]; 
  char UniqueID[64]; 
  unsigned long long NUID; 
  char Reserved[512]; 
}; 

typedef struct DDDesc DDDesc_t;



int cbIgnoreInstaCal();
int cbErrHandling(int ErrReporting, int ErrHandling);
int cbGetDaqDeviceInventory(int InterfaceType, struct DDDesc *Inventory, int *NumberOfDevices);
int cbCreateDaqDevice(int BoardNum, struct DDDesc deviceDescriptor);
int cbDConfigBit(int BoardNum, int PortType, int BitNum, int Direction);
int cbDBitOut(int BoardNum, int PortType, int BitNum, unsigned short BitValue);
int cbDBitIn(int BoardNum, int PortType, int BitNum, unsigned short *BitValue);
int cbAIn(int BoardNum, int Channel, int Range, unsigned short *DataValue);
int cbReleaseDaqDevice(int BoardNum);
int cbAInScan(int BoardNum, int LowChan, int HighChan, long Count, long *Rate, int Range, int MemHandle, int Options);
long cbWinBufAlloc(long NumPoints);
int cbWinBufToArray(int MemHandle, unsigned short* DataArray, long FirstPoint, long Count);
int cbWinBufFree(int MemHandle);
int cbAOut(int BoardNum, int Channel, int Range, int DataValue);
int cbVOut(int BoardNum, int Channel, int Range, float DataValue, int Options);
int cbGetConfig(int InfoType, int BoardNum, int DevNum, int ConfigItem, int *ConfigVal);
int cbDConfigPort(int BoardNum, int PortType, int Direction);
int cbAInputMode(int BoardNum, int InputMode);
int cbSetConfig(int InfoType, int BoardNum, int DevNum, int ConfigItem, int ConfigVal);
  
  
  
  
  
void BP_cbIgnoreInstaCal(int *out)
{
	int y;
	y=cbIgnoreInstaCal();
  *out=y;
}

void BP_cbErrHandling(int *ErrReporting, int *ErrHandling, int *out)
{
  int y;
  y=cbErrHandling(*ErrReporting, *ErrHandling);
  *out=y;
}

void BP_ListDevices(char **ProductName, char **SerialID, int *nbdevice, int *out)
{
 
  int y;
  int nbdev;
  int InterfaceType=1;
  DDDesc_t *dum=malloc(3 * sizeof(*dum));
  nbdev=3;
  y=cbGetDaqDeviceInventory(InterfaceType, dum, &nbdev);
  for (int k=0; k<nbdev; k++){
    strcpy(ProductName[k],dum[k].ProductName);
    strcpy(SerialID[k],dum[k].UniqueID);
    //printf("Serial de %d est %s\n",k,dum[k].UniqueID);
  }
  *nbdevice=nbdev;
  *out=y;
  free(dum);
  
}

void BP_InitDAQ(char *ProductName[64], char *SerialID[64], int *BoardNum, int *out)
{
  //D'abord voir si le ProductName/SerialID est disponible
  
  int y;
  int nbdev;
  int InterfaceType=1;
  DDDesc_t *dum=malloc(3 * sizeof(*dum));
  int letest;
  nbdev=3;
  y=cbGetDaqDeviceInventory(InterfaceType, dum, &nbdev);
  int ladev=-1;
  for (int k=0; k<nbdev; k++ ){
    letest=strcmp(dum[k].ProductName, *ProductName); 
    if (letest==0){
      letest=strcmp(dum[k].UniqueID, *SerialID); 
      if (letest==0){
        ladev=k;
        y= cbCreateDaqDevice(*BoardNum, dum[k]);
      }
    }
  }
  *out=y;
  free(dum);
}

void BP_cbReleaseDaqDevice(int *BoardNum, int *out)
{
  int y;
  y=cbReleaseDaqDevice(*BoardNum);
  *out=y;
}
 
 
void BP_cbAInputMode(int *BoardNum, int *InputMode, int *out)
{
  int y;
  y=cbAInputMode(*BoardNum, *InputMode);
  *out=y;  
}
  
  

void BP_cbDConfigPort(int *BoardNum, int *PortType, int *Direction,int *out)
// Direction = 1 pour out; = 2 pour in
{
  int y;
  y=cbDConfigPort(*BoardNum, *PortType,*Direction);
  *out=y;
}  
  
 
   

void BP_cbDConfigBit( int *BoardNum, int *PortType, int *BitNum, int *Direction, int *out)
// Direction = 1 pour out; = 2 pour in
{
  int y;
  y=cbDConfigBit(*BoardNum, *PortType, *BitNum, *Direction);
  *out=y;
}


void BP_cbDBitOut(int *BoardNum, int *PortType, int *BitNum, int *BitValue, int *out)
{
  int y;
  y=cbDBitOut(*BoardNum, *PortType, *BitNum, *BitValue);
  *out=y;
}

void BP_cbDBitIn(int *BoardNum, int *PortType, int *BitNum, int *BitValue, int *out)
{
  int y;
  unsigned short lavaleur;
  y=cbDBitIn(*BoardNum,*PortType,*BitNum,&lavaleur);
  *BitValue=(int) lavaleur;
  *out=y;
}

void BP_cbGetConfig(int *InfoType, int *BoardNum, int *DevNum, int *ConfigItem, int *ConfigVal, int *out)
{
  int y;
  int lavaleur;
  y=cbGetConfig(*InfoType,*BoardNum,*DevNum,*ConfigItem,&lavaleur);
  *ConfigVal=(int) lavaleur;
  *out=y;
}

void BP_cbSetConfig(int *InfoType, int *BoardNum, int *DevNum, int *ConfigItem, int *ConfigVal, int *out)
{
  int y;
  y=cbSetConfig(*InfoType,*BoardNum,*DevNum,*ConfigItem,*ConfigVal);
  *out=y;
}  


void BP_cbAIn(int *BoardNum, int *Channel, int *Range, int *DataValue, int *out)
{
  int y;
  unsigned short lavaleur;
  y=cbAIn(*BoardNum, *Channel, *Range, &lavaleur);
  *DataValue=(int) lavaleur;
  *out=y;
}

void BP_cbAInScan(int *BoardNum, int *LowChan, int *HighChan, long *Count, long *Rate, int *Range, int *DataValue, int *out ){
  int y;
  unsigned short *DataArray=NULL;
  int MemHandle=0;
  MemHandle=cbWinBufAlloc(*Count);
  DataArray = (unsigned short*)MemHandle;
  y=cbAInScan(*BoardNum, *LowChan, *HighChan, *Count, Rate, *Range, MemHandle,0);
  for (int k=0; k<*Count; k++) DataValue[k]=DataArray[k];
  cbWinBufFree(MemHandle);
  DataArray=NULL;
  *out=y;
}

void BP_cbAOut(int *BoardNum, int *Channel, int *Range, int *DataValue, int *out){
  int y=0;
  y=cbAOut(*BoardNum, *Channel, *Range, (unsigned short)*DataValue);
  *out=y;
}


void BP_cbVOut(int *BoardNum, int *Channel, int *Range, float *DataValue, int *out){
  int y=0;
  y=cbVOut(*BoardNum, *Channel, *Range, *DataValue, 0);
  *out=y;
}
int cbVOut(int BoardNum, int Channel, int Range, float DataValue, int Options);