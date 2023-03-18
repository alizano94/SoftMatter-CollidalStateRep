from src.control import *

#Hyperparameter definitions
pxs = 400

#FLAGS definitions

#Extra data
dr = True
metric='cosine'
n_components=2
n_neighbors=10
min_dist=0.5
feat_file = 'ae_features.csv'
clust_file = 'ae_clusters.csv'

#Start main code
control = RL(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500
)
if dr:
    control.umap(
        load_file='ae_features.csv',
        out_file='ae_2d_features.csv',
        metric=metric,
        n_components=n_components,
        n_neighbors=n_neighbors,
        min_dist=min_dist
    )
control.plot_2D_cluster_mappings(
    coordinates='ae_2d_features.csv',
    clusters='ae_clusters.csv',
    save_file='HDBSCAN_UMAP_2D_plot.png',
)
