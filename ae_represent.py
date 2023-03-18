from src.control import *

#Hyperparameter definitions
pxs = 224

#FLAGS definitions

#Extra data
hierarchichal = False
min_clust_size = 200
min_samples = 20
eps = 10
metric = 'cosine'
feat_file = 'ae_features.csv'
clust_file = 'ae_clusters.csv'

#Start main code
control = RL(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500
)
if hierarchichal:
    control.cluster_hdbscan(
        minimum_cluster_size = min_clust_size,
        minimum_samples = min_samples,
        epsilon = eps,
        metric = metric,
        load_file = feat_file,
        out_file = clust_file
    )
else:
    control.cluster_dbscan(
        min_samples = min_samples,
        epsilon = eps,
        metric = metric,
        load_file = feat_file,
        out_file = clust_file
    )
