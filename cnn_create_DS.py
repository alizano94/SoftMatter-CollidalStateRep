from src.control import *

#Hyperparameter definitions
pxs = 224

#FLAGS definitions

#Extra data
val=0.1
test=0.1
clust_file = 'ae_clusters.csv'

#Start main code
control = RL(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500
)
control.getCNNDS(
    load_file=clust_file,
    validation_split=val,
    testing_split=test
)
