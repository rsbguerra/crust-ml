
Les tests sont organisés en différentes catégories :

  syntax/bad/    refusés à l'analyse lexicale ou syntaxique
  syntax/good/   acceptés à l'analyse syntaxique
  typing/bad/    refusés au typage
  typing/good/   acceptés au typage
  typing2/bad/   refusés au typage des ressources
  typing2/good/  acceptés au typage des ressources
  exec-fail/     compilés avec succès mais l'exécution doit échouer
  exec/          compilés et exécutés avec succès, le résultat attendu étant
                 conforme au fichier .out

Les tests sont cumulatifs i.e.

- les fichiers de typing/{good,bad}/, typing2/{good,bad}/, exec-fail/
  et exec/ peuvent être utilisés pour la catégorie syntax/good/

- les fichiers de typing2/{good,bad}/, exec-fail/ et exec/ peuvent être
  utilisés pour la catégorie typing/good/

- les fichiers de exec-fail/ et exec/ peuvent être
  utilisés pour la catégorie typing2/good/
