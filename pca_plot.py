from src.control import *

#Hyperparameter definitions
pxs = 400

#FLAGS definitions

#Extra data
dr = False
min_clust_size = 140
min_samples = 85
eps = 0.24
metric = 'euclidean'
feat_file = 'ae_features.csv'
clust_file = 'ae_clusters.csv'

#Start main code
control = RL(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500
)
if dr:
    control.pca(
        load_file='ae_features.csv',
        out_file='ae_2d_features.csv',
        n_components=2,
    )
control.plot_2D_cluster_mappings(
    coordinates='ae_2d_features.csv',
    clusters='ae_clusters.csv',
    save_file='HDBSCAN_PCA_2D_plot.png',
    positions=None,
    polar=False,
    annotate = False,
    tag = 'PCA',
)
