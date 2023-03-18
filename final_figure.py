from src.control import *
import matplotlib.pyplot as plt
from matplotlib.offsetbox import (OffsetImage, AnnotationBbox)
import cv2
from PIL import Image
import pandas as pd
from adjustText import adjust_text

plt.rcParams["font.family"] = "Times New Roman"
plt.rcParams['figure.constrained_layout.use'] = True

pxs = 224
control = RL(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500,
    g=0
)
control.createCNN(
    k=6,
    summary=False
    )
control.loadCNN()


s_labels = [
    '$S_{0}$',
    '$S_{1}$',
    '$S_{2}$',
    '$S_{3}$',
    '$S_{4}$',
    '$S_{5}$',
]
configs = [
    'V1-T2-75step10s.png',
    'V1-T11-16step10s.png',
    'V4-T15-1step10s.png',
    'V4-T28-74step10s.png',
    'V4-T12-48step10s.png',
    #'V4-T25-100step10s.png',
    'V4-T13-12step10s.png',
    'V3-T21-10step10s.png',
    'V4-T45-89step10s.png'
]
tags = [
    'Amorphous',
    'Amorphous',
    'Mixed',
    'Grain Defect',
    'Polycrystalline',
    'Void Defective',
    'Crystalline 1',
    'Crystalline 2'
]
lettters = [
    'a)',
    'b)',
    'c)',
    'd)',
    'e)',
    'f)',
    'g)',
    'h)'
]
n=20

fig = plt.figure(
    constrained_layout=True,
    figsize=(6, 12),
    )
subfigs = fig.subfigures(
    nrows=1,
    ncols=1,
)
axsleft = subfigs.subplots(
    ncols = 2,
    nrows = len(configs)//2
)

for k in range(len(configs)):
    if k < 4:
        i = k
        j = 0
    else:
        i = k - 4
        j = 1
    ax = axsleft[i][j]
    ax.get_yaxis().set_ticks([])
    ax.get_xaxis().set_ticks([])
    img = os.path.join(
        control.cnn_ds_path,
        'unclassified_raw_data',
        'full',
        configs[k]
    )
    s = control.runCNN(
        img
    )
    img = cv2.imread(
        img
    )
    ax.imshow(
        img,
    )
    ax.annotate(
        tags[k],
        xy=(780,120),
        color='black',
        weight='bold',
        horizontalalignment='right'
    )
    ax.annotate(
        lettters[k],
        xy=(50,120),
        color='black',
        weight='bold'
    )
    anotation = 'S = '+str(s[0])
    ax.annotate(
        anotation,
        xy=(780,750),
        color='black',
        weight='bold',
        horizontalalignment='right'
    )
plt.savefig(
    os.path.join(
        control.cnn_results_path,
        'figure7.png'
    ),
    dpi=300
)