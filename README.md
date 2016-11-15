run it with sbt:

sbt -mem 15000 "run <base_dir> <classifier> <mode>"

where: 

<base_dir> is the folder witch must contain other folders 
with zip archive respectively: 
/train - train.zip
/validation - validation.zip
/test - test.zip

<classifier> has following types: 
nb - Naive Bayes
lsvm - Linear Support Vector Machine
lg - Logistic Regression

<mode> can be either:
vali
test

-mem 15000 sets memory to 15G for performance reasons, 
so that all hash maps fits RAM in memory