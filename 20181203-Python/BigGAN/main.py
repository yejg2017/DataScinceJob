
import os,sys
sys.path.append("D:/DataScience-job/20181203-Python/BigGAN/")

from parameter import *
from trainer import Trainer
# from tester import Tester
from data_loader import Data_Loader
from torch.backends import cudnn
from utils import make_folder

import glob


def main(config):
    # For fast training
    cudnn.benchmark = True


    config.n_class = len(glob.glob(os.path.join(config.image_path, '*/')))
    print('number class:', config.n_class)
    # Data loader
    data_loader = Data_Loader(config.train, config.dataset, config.image_path, config.imsize,
                             config.batch_size, shuf=config.train)

    # Create directories if not exist
    make_folder(config.model_save_path, config.version)
    make_folder(config.sample_path, config.version)
    make_folder(config.log_path, config.version)
    make_folder(config.attn_path, config.version)


    print('config data_loader and build logs folder')

    if config.train:
        if config.model=='sagan':
            trainer = Trainer(data_loader.loader(), config)
        elif config.model == 'qgan':
            trainer = qgan_trainer(data_loader.loader(), config)
        trainer.train()
    else:
        tester = Tester(data_loader.loader(), config)
        tester.test()

if __name__ == '__main__':
    config = get_parameters()
    print(config)
    main(config)


import numpy as np
from scipy.stats import entropy
from numpy.linalg import norm
from scipy import linalg
x=np.random.rand(20,3)
y=np.random.rand(30,3)
def fid(X, Y):
    m = X.mean(0)
    m_w = Y.mean(0)
    X_np = X.copy()
    Y_np = Y.copy()

    C = np.cov(X_np.transpose())
    C_w = np.cov(Y_np.transpose())
    C_C_w_sqrt = linalg.sqrtm(C.dot(C_w), True).real

    score = m.dot(m) + m_w.dot(m_w) - 2 * m_w.dot(m) + \
        np.trace(C + C_w - 2 * C_C_w_sqrt)
    return np.sqrt(score)

print(fid(x,y))


from sklearn.metrics.pairwise import polynomial_kernel
def get_splits(n, splits=10, split_method='openai'):
    if split_method == 'openai':
        return [slice(i * n // splits, (i + 1) * n // splits)
                for i in range(splits)]
    elif split_method == 'bootstrap':
        return [np.random.choice(n, n) for _ in range(splits)]
    else:
        raise ValueError("bad split_method {}".format(split_method))

z=get_splits(3,split_method="bootstrap")
