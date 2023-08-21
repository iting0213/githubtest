texts = []
labels = []

import csv

with open('True.csv', newline='', encoding="utf-8", errors='ignore') as csvfile:
    trues = csv.reader(csvfile)
    for true in trues:
        texts.append(str(true).lower())
        labels.append('T')

with open('Fake.csv', newline='', encoding="utf-8", errors='ignore') as csvfile:
    fakes = csv.reader(csvfile)
    for fake in fakes:
        texts.append(str(fake).lower())
        labels.append('F')
        
from sklearn.feature_extraction.text import TfidfVectorizer

TFIDF_vectorizer = TfidfVectorizer(min_df=1, stop_words='english')
#TFIDF_vectorizer = TfidfVectorizer(min_df=1)
TFIDF_vectors = TFIDF_vectorizer.fit_transform(texts)

from sklearn.decomposition import TruncatedSVD

svd_model = TruncatedSVD(n_components = 15)
SVD_vectors = svd_model.fit_transform(TFIDF_vectors)

x = SVD_vectors[::]
y = labels[::]

#Classify with SVM Method
from sklearn.svm import SVC

SVM_model = SVC(kernel='linear', C=1.0)
SVM_model.fit(x,y)

from sklearn.model_selection import train_test_split
# Split 10% of training set for evaluation
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.1, random_state=0)

predicted_results = []
expected_results = []

expected_results.extend(y_test)
predicted_results.extend(SVM_model.predict(x_test))

from sklearn import metrics
print(metrics.classification_report(expected_results, predicted_results))

texts_test = []
labels_test = []

with open('testData_true.csv', newline='', encoding="utf-8", errors='ignore') as csvfile:
    trues = csv.reader(csvfile)
    for true in trues:
        texts_test.append(str(true).lower())
        labels_test.append('T')
        
with open('testData_false.csv', newline='', encoding="utf-8", errors='ignore') as csvfile:
    fakes = csv.reader(csvfile)
    for fake in fakes:
        texts_test.append(str(fake).lower())
        labels_test.append('F')
        
from sklearn.feature_extraction.text import TfidfVectorizer

TFIDF_vectorizer_test = TfidfVectorizer(min_df=1, stop_words='english')
#TFIDF_vectorizer_test = TfidfVectorizer(min_df=1)
TFIDF_vectors_test = TFIDF_vectorizer.fit_transform(texts_test)

from sklearn.decomposition import TruncatedSVD

svd_model = TruncatedSVD(n_components = 15)
SVD_vectors_test = svd_model.fit_transform(TFIDF_vectors_test)

predicted_results = []
expected_results = []

expected_results.extend(labels_test)
predicted_results.extend(SVM_model.predict(SVD_vectors_test))

from sklearn import metrics
print(metrics.classification_report(expected_results, predicted_results))

texts_test2 = []
labels_temp = []
labels_test2 = []