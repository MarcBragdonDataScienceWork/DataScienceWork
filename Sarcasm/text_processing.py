import itertools
import log
import re
import nltk
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
import string
#from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator
import matplotlib.pyplot as plt
import ssl


class TextProcessing:
    logger = log.setup_custom_logger('Text Processing')

    def __init__(self):
        '''
        On most systems, this shouldn't be required, but if you are running
        this on a mac, you have to get around the cert check for ntlk Download
        and this is by far the easiest way.
        '''
        try:
            _create_unverified_https_context = ssl._create_unverified_context
        except AttributeError:
            pass
        else:
            ssl._create_default_https_context = _create_unverified_https_context
        """
        Downloading the lexicon
        """
        nltk.download('punkt')
        nltk.download('stopwords')
        nltk.download('wordnet')


    def tokenize_text(self, text, n_grams):
        bi_grams = list(nltk.ngrams(text.split(' '), n_grams))
        bi_gram_list = []
        for tuple in bi_grams:
            bi_gram_list.append(" ".join(tuple))
        return bi_gram_list

    """
    return a list of tokens representing all the words in the tweet

    def tokenize_text(self, text):
        tokens = nltk.word_tokenize(text)
        return tokens
    """
    """
    Expects a list of tokens will remove the stop words and return that list
    with stop words removed
    """
    def remove_stop_words(self, tokens):
        self.logger.debug("Removing the Stop Words")
        stop = stopwords.words('english')
        output_tokens = []
        for token in tokens:
            #if token has a space, it's a tuple
            if(token.find(' ')):
                sub_tokens = [s_token for s_token in token.split(' ') if s_token not in stop]
                #bring the tuple back together
                output_tokens.append(" ".join(sub_tokens))
            #if there is no space add it only if not in stop
            else:
                if(token not in stop):
                    output_tokens.append(token)
        #tokens = [token for token in tokens if token not in stop]
        # logger.debug("These are the tokens %s" % tokens)
        return output_tokens
    """
    Removes all tokens that have a length less than the provided word_size argument
    """
    def remove_small_words(self, tokens, word_size):
        self.logger.debug('Removing all tokens smaller than size %d' % word_size)
        tokens = [word for word in tokens if len(word) >= word_size]
        return tokens
    """
    remove all the punctuation and special characters from the tweet
    """
    def remove_punctuation(self,  tokens):
        self.logger.debug('Removing all punctuation')
        tokens = [token.translate(str.maketrans('', '', string.punctuation)) for token in tokens]
        return tokens
    """
    make everything lower case
    """
    def make_lower_case(self, tokens):
        self.logger.debug('Changing to Lower Case')
        tokens = [word.lower() for word in tokens]
        return tokens
    """
    take the word stem
    """
    def stem_tokens(self, tokens):
        self.logger.debug('Using WordNetLemmatizer to stem words')
        lmtzr = WordNetLemmatizer()
        tokens = [lmtzr.lemmatize(word) for word in tokens]
        return tokens
    """
    Collapse a list of listts into a single list

    def flatten_list(self, list_of_lists):
        return list(itertools.chain(*list_of_lists))
    """
    """
    This method could use a little optimization but for now it works fine
    takes a list, joins the list of strings together, makes a list of sentences.
    """
    def flatten_list(self, list_of_lists):
        return_list = []
        for list in list_of_lists:
            return_list.append(' '.join(list))
        return return_list
    """
    Remove all hyperlinks
    """
    def remove_links(self, tweet):
        return re.sub(r"http\S+", "", tweet)

    """
    Make a word cloud
    
    def make_word_cloud(self, list_of_words):
        text = ' '.join(list_of_words)
        #Create the word cloud image
        wordcloud = WordCloud().generate(text)
        #Display it
        plt.imshow(wordcloud, interpolation = 'bilinear')
        plt.axis("off")
        plt.show()
	"""
    """
    Remove specific word
    """
    def remove_words(self, tokens, list_of_words):
        self.logger.debug("Removing words " + str(list_of_words))
        tokens = [word for word in tokens if word not in list_of_words]
        return tokens
