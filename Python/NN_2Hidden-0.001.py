import numpy as np
import pandas as pd
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import StandardScaler
from lasagne.layers import DenseLayer
from lasagne.layers import InputLayer
from lasagne.layers import DropoutLayer
from lasagne.layers import *
from lasagne.nonlinearities import softmax
from lasagne.updates import nesterov_momentum
from lasagne.updates import *
from nolearn.lasagne import NeuralNet
import theano
from sklearn.multiclass import OneVsRestClassifier
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC

def load_train_data(path):
    df = pd.read_csv(path)
    X = df.values.copy()
    np.random.shuffle(X)
    X, labels = X[:, 1:-1].astype(np.float32), X[:, -1]
    encoder = LabelEncoder()
    y = encoder.fit_transform(labels).astype(np.int32)
    #scaler = StandardScaler()
    #X = scaler.fit_transform(X)
    return X, y, encoder#, scaler

def load_test_data(path):
    df = pd.read_csv(path)
    X = df.values.copy()
    X, ids = X[:, 1:].astype(np.float32), X[:, 0].astype(str)
    #X = scaler.transform(X)
    return X, ids
def float32(k):
    return np.cast['float32'](k)

class AdjustVariable(object):
    def __init__(self, name, start=0.03, stop=0.001):
        self.name = name
        self.start, self.stop = start, stop
        self.ls = None

    def __call__(self, nn, train_history):
        if self.ls is None:
            self.ls = np.linspace(self.start, self.stop, nn.max_epochs)

        epoch = train_history[-1]['epoch']
        new_value = float32(self.ls[epoch - 1])
        getattr(nn, self.name).set_value(new_value)


def make_submission(clf, X_test, ids, encoder, name='my_neural_net_submission.csv'):
    y_prob = clf.predict_proba(X_test)
    with open(name, 'w') as f:
        f.write('id,')
        f.write(','.join(encoder.classes_))
        f.write('\n')
        for id, probs in zip(ids, y_prob):
            probas = ','.join([id] + map(str, probs.tolist()))
            f.write(probas)
            f.write('\n')
    print("Wrote submission to file {}.".format(name))

np.random.seed(1234567)
#X, y, encoder, scaler = load_train_data('data/train.csv')
X, y,encoder= load_train_data('../Data/train.csv')
X_test, ids = load_test_data('../Data/test.csv')
#X=np.log(X+1)
X=np.sqrt(X+(3/8))
#X_test=np.log(X_test+1)
X_test=np.sqrt(X_test+(3/8))
print X[1]
print y[1]
num_classes = len(encoder.classes_)
num_features = X.shape[1]

layers0 = [('input', InputLayer),
('dropoutf', DropoutLayer),
('dense0', DenseLayer),
('dropout', DropoutLayer),
('dense1', DenseLayer),
('dropout2', DropoutLayer), 
('output', DenseLayer)]

for i in range(1, 10):
    
    net0 = NeuralNet(layers=layers0,

    input_shape=(None, num_features),
    dropoutf_p=0.15,
    dense0_num_units=1000,
    dropout_p=0.25,
    dense1_num_units=500,
    dropout2_p=0.18,#0.25,

    output_num_units=num_classes,
    output_nonlinearity=softmax,

    #update=nesterov_momentum,
    update=adagrad,
    update_learning_rate=0.005,
    #update_momentum=0.9, only used with nesterov_
    eval_size=0.02,
    verbose=1,
    max_epochs=100)



   
    net0.fit(X, y)
    nombre="../Output/NN/NN_2hidden-0.001prueba"+str(i)+".csv"
    make_submission(net0, X_test, ids, encoder,nombre)

    
