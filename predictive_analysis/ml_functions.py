import os

from zipfile import ZipFile
import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelBinarizer
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score


class DataSaveload():
    def __init__(self, data_path: str, ArchName: str, filename: str):
        self.data_path = data_path
        self.filename = filename
        self.ArchName = ArchName

    def save_data(self):
        os.makedirs(self.data_path, exist_ok=True)
        if self.ArchName.endswith('.zip'):
            zip_path = os.path.join(os.getcwd(), self.ArchName)
            with ZipFile(zip_path, 'r') as zip:
                zip.printdir()
                zip.extractall(path=self.data_path)
                print('Finished!')
        else:
            os.path.join(self.data_path, self.filename)

    def load_data(self):
        arch = os.path.join(self.data_path, self.filename)
        return pd.read_csv(arch)

    def load_save(self):
        self.savedata()
        return self.loadata()


class BinaryTransformer(BaseEstimator, TransformerMixin):
    def __init__(self, sparse_output=False):
        self.sparse_output = sparse_output

    def fit(self, X, y=None):
        return self

    def transform(self, X, y=None):
        binarizer = LabelBinarizer(sparse_output=self.sparse_output)
        return binarizer.fit_transform(X)


def display_errors(labels, predictions):
    print('RMSE:', mean_squared_error(labels, predictions, squared=False))
    print('MAE:', mean_absolute_error(labels, predictions))


def dispaly_accuracy(labels, predictions):
    errors = abs(labels - predictions)
    mape = 100 * np.mean(errors / labels)
    accuracy = 100 - mape
    print(f"Accuracy: {accuracy:5.2f} %")
