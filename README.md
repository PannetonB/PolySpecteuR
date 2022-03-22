# PolySpecteuR 4.0

Version plus modulaire des outils pour le contrôle du SpectrAAC2

Tous les fichiers et programmes nécessaires sont inclus dans ce projet RStudio.

L'utilisateur doit créé ses propres fichiers de configuration d'instrument et de paramètres d'acquisition selon les besoins d'une session de prise de données.

L'utilisateur doit créé un fichier du type mainXXXX.R (voir exemples et explications dans le répertoire R du projet) définissant la session de prise de données.

ATTENTION: le fichier ***mccdaq.exe*** pour installer les bibliothèques MCDAQ (Measurement instruments) n'est pas inclus. Il est trop volumineux pour **GitHub**. Par contre, les fichiers ***cbw.h***, ***cbw64.dll*** de ***DaqDevInfo64.dll*** sont joints dans le répertoire ***C***du projet. Cela devrait suffire.

B. Panneton - Avril 2022.
