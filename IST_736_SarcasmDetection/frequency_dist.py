import pandas as pd
import log
import csv
logger = log.setup_custom_logger('freq_dist')

def process_data(filename):
    word_counts = {}
    logger.info("Starting Read of File")
    line_count = 0
    with open(filename) as infile:
        for line in infile:
            line_count = line_count + 1
            if line_count % 100000 == 0:
                logger.info("100000 Lines read")
            for word in line.split(',')[1].split(' '):
                if word in word_counts.keys():
                    logger.debug('Word Exists')
                    #if we have the word, increment the count
                    count = word_counts[word]
                    logger.debug(word + ' exists {} times.'.format(count))
                    word_counts[word] = count + 1
                else:
                    #if we don't have the word add it set the count to one
                    word_counts[word] = 1
    return word_counts



def write_data(filename, data):
    logger.info("Writing Data to CSV")
    csv_columns = ['Word', 'Count']
    try:
        with open('output.csv', 'w', newline='') as csvfile:
            writer = csv.writer(csvfile, delimiter=' ',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
            for row in data:
                print(row)
                writer.writerow(row)
    except IOError:
        print("I/O error")


clean_data = process_data('data/train-balanced-sarcasm.csv')
#print(clean_data)
write_data('output.csv', clean_data)
