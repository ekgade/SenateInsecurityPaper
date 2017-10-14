from collections import defaultdict
import sys
import re
from string import punctuation

@outputSchema("URLs:chararray")
def pickURLs(url):
    try:
        names =set([
            'csce',
            'tomudall',
            'wicker',
            'menendez',
            'warner',
            'moran',
            'blunt',
            'portman',
            'flake',
            'murphy',
            'hirono',
            'king',
            'cardin',
            'warren',
            'fischer',
            'heller',
            'heinrich',
            'gillibrand',
            'heitkamp',
            'cruz',
            'sanders',
            'akaka',
            'alexander',
            'allard',
            'allen',
            'ayotte',
            'barrasso',
            'baucus',
            'bayh',
            'begich',
            'bennet',
            'bennett',
            'biden',
            'bingaman',
            'blumenthal',
            'bond',
            'boxer',
            'brown',
            'brownback',
            'bunning',
            'burns',
            'burr',
            'burris',
            'byrd',
            'cantwell',
            'carper',
            'casey',
            'chafee',
            'chambliss',
            'clinton',
            'coats',
            'coburn',
            'cochran',
            'coleman',
            'collins',
            'conrad',
            'coons',
            'corker',
            'cornyn',
            'corzine',
            'craig',
            'crapo',
            'dayton',
            'demint',
            'dewine',
            'dodd',
            'dole',
            'domenici',
            'dorgan',
            'durbin',
            'ensign',
            'enzi',
            'feingold',
            'feinstein',
            'franken',
            'frist',
            'graham',
            'grassley',
            'gregg',
            'hagan',
            'hagel',
            'harkin',
            'hatch',
            'hoeven',
            'hutchison',
            'inhofe',
            'inouye',
            'isakson',
            'jeffords',
            'johanns',
            'johnson',
            'ronjohnson',
            'kaufman',
            'kennedy',
            'kerry',
            'kirk',
            'klobuchar',
            'kohl',
            'kyl',
            'landrieu',
            'lautenberg',
            'leahy',
            'lee',
            'lemieux',
            'levin',
            'lieberman',
            'lincoln',
            'lott',
            'lugar',
            'manchin',
            'martinez',
            'mccain',
            'mccaskill',
            'mcconnell',
            'merkley',
            'mikulski',
            'murkowski',
            'murray',
            'billnelson',
            'bennelson',
            'obama',
            'paul',
            'pryor',
            'reed',
            'reid',
            'risch',
            'roberts',
            'rockefeller',
            'rubio',
            'salazar',
            'santorum',
            'sarbanes',
            'schatz',
            'schumer',
            'sessions',
            'shaheen',
            'shelby',
            'smith',
            'snowe',
            'specter',
            'stabenow',
            'stevens',
            'sununu',
            'talent',
            'tester',
            'thomas',
            'thune',
            'toomey',
            'vitter',
            'voinovich',
            'webb',
            'whitehouse',
            'wyden'])
        
        regexp = re.compile('([a-z]*)(\/([a-z]+))?')

        results = []

        result = regexp.search(url)
            if result is not None:
                if len(result.group(1)):
                    if result.group(1) in names:
                        return(result.group(1))
                if result.group(2) is not None:
                    if result.group(3) in names:
                        return(result.group(3))

    except:
        pass
    return 'other'


@outputSchema('counts:bag{tuple(word:chararray,count:int)}')
def Threat_countWords(content):
    try:
        Threat_Words = set([
            'afterlife',
            'agonstic',
            'alla',
            'allah',
            'altar',
            'amen',
            'amish',
            'angel',
            'angelic',
            'angels',
            'baptist',
            'baptize',
            'belief',
            'bible',
            'biblic',
            'bishop',
            'bless',
            'buddha',
            'catholic',
            'chapel',
            'chaplain',
            'christ',
            'christen',
            'christain',
            'christmas',
            'church',
            'clergy',
            'confess',
            'convents',
            'crucify',
            'demon',
            'demonic',
            'demons',
            'devil',
            'divine',
            'doom',
            'episcopal',
            'evangelic',
            'faith',
            'fundamentalist',
            'gentile',
            'god',
            'goddess',
            'gospel',
            'hashanal',
            'heaven',
            'hell\s',
            'hellish',
            'hells',
            'hindu',
            'holier',
            'holiest',
            'holy',
            'hymn',
            'imam',
            'immoral',
            'immortal',
            'islam',
            'jesuit',
            'jesus',
            'jew',
            'jewish',
            'juda',
            'karma',
            'kippur',
            'koran',
            'kosher',
            'lord',
            'lutheran',
            'mecca',
            'meditate',
            'mennonite',
            'mercifull',
            'mercy',
            'methodist',
            'minister',
            'ministry',
            'missionary',
            'mitzvah',
            'mohammad',
            'monastry',
            'monk',
            'moral',
            'morality',
            'morals',
            'mormon',
            'mosque',
            'muhammed',
            'mujahids',
            'muslim',
            'nun',
            'orthodox',
            'pagan',
            'papal',
            'paradise',
            'passover',
            'pastor',
            'penance',
            'pentecost',
            'pew',
            'piet',
            'pilgrim',
            'pious',
            'pope',
            'prayer',
            'preach',
            'presbyterian',
            'priest',
            'prophet',
            'protestant',
            'puritan',
            'quran',
            'rabbi',
            'rabbinica',
            'ramadan',
            'religion',
            'rite',
            'ritual',
            'rosary',
            'sabbath',
            'sacred',
            'sacrifice',
            'saint',
            'salvatior',
            'satan',
            'scripture',
            'sect',
            'sectarian',
            'seminary',
            'shia',
            'shiite',
            'shrine',
            'sikh',
            'sin',
            'sinner',
            'soul',
            'spirit',
            'sunni',
            'temple',
            'testament',
            'theology',
            'torah',
            'vatican',
            'veil',
            'worship',
            'yiddish',
            'zen',
            'zion',
            'afraid',
            'alarm',
            'anguish',
            'anxiety',
            'apprehension',
            'aversion',
            'bewilderment',
            'confusion',
            'desperate',
            'discomfort',
            'distraught',
            'distress',
            'disturb',
            'dread',
            'emotional',
            'fear',
            'feared',
            'fearing',
            'fears',
            'frantic',
            'fright',
            'hesitant',
            'horrific',
            'horrible',
            'humiliating',
            'impatient',
            'inadequate',
            'insecure',
            'irritation',
            'misery',
            'numerous',
            'obsession',
            'obsess',
            'overwhelm',
            'panic',
            'petrify',
            'pressure',
            'reluctant',
            'restless',
            'saw',
            'scare',
            'shake',
            'shy',
            'sicken',
            'startle',
            'strain',
            'stress',
            'stunned',
            'stuns',
            'tense',
            'tension',
            'terrified',
            'terrifying',
            'terror',
            'tremble',
            'turmoil',
            'uncertain',
            'uncomfortable',
            'uneasy',
            'unsure',
            'upset',
            'vulnerable',
            'worry'])
    except:
        pass
    threat_counts = defaultdict(int)
    threat_counts['total'] = 0

    if not content or not isinstance(content, unicode):
        return [(('total'), 0)]
    splitcontent = content.lower().split()
# splitcontent = content.lower().translate(None, punctuation).split()
    threat_counts['total'] = len(splitcontent)
    for word in splitcontent:
        if word in Threat_Words:
            threat_counts[word] += 1

    # Convert counts to bag
    countBag = []
    for word in threat_counts.keys():
        countBag.append( (word, threat_counts[word] ) )
    return countBag




@outputSchema("counts:bag{tuple(year:int, month:int, word:chararray, count:int, filled:int, afterLast:int, URLs:chararray)}")
#@outputSchema("counts:bag{tuple(year:int, month:int, word:chararray, count:int, filled:int, URLs:chararray)}")
def fillInCounts(data):
    try:
        outBag = []
        firstYear = 2013
        firstMonth = 9
        lastYear = 0
        lastMonth = 0
        # used to compute averages for months with multiple captures
        # word -> (year, month) -> count
        counts = defaultdict(lambda : defaultdict(list))
        # (year,month) -> date
        lastCaptureOfMonth = defaultdict(int)
        # word -> (year,month) -> {date, count}
        endOfMonthCounts = defaultdict(lambda : defaultdict(lambda: dict({'date':0,'count':0})))
        seenDates = {}
        #for s in src:
        #  maxSeenDate= max(date)
        for (src, date, wordCounts, urls) in data:
            for (word, countTmp) in wordCounts:
                year = int(date[0:4])
                month = int(date[4:6])
                # not sure what's going on, this is temporary fix
                if isinstance(countTmp,str) or isinstance(countTmp,int):
                    count = int(countTmp)
                else:
                    continue
                ymtup = (year, month)
                counts[word][ymtup].append(count)
                if date > lastCaptureOfMonth[ymtup]:
                    lastCaptureOfMonth[ymtup] = date
                if date > endOfMonthCounts[word][ymtup]['date']:
                    endOfMonthCounts[word][ymtup]['date'] = date
                    endOfMonthCounts[word][ymtup]['count'] = count
                seenDates[(year,month)] = True
                if year < firstYear:
                    firstYear = year
                    firstMonth = month
                elif year == firstYear and month < firstMonth:
                    firstMonth = month
                elif year > lastYear:
                    lastYear = year
                    lastMonth = month
                elif year == lastYear and month > lastMonth:
                    lastMonth = month
        for word in counts.keys():
            # The data was collected until Sep 2013
            years = range(firstYear, 2014)
            useCount = 0
            afterLast = False
            filled = False
            ymLastUsed = (0,0)
            for y in years:
                if y > lastYear:
                    afterLast = True
                if y == firstYear:
                    mStart = firstMonth
                else:
                    mStart = 1
                if y == 2013:
                    mEnd = 9
                else:
                    mEnd = 12
                for m in range(mStart, mEnd+1):
                    if y == lastYear and m > lastMonth:
                #afterLast = True
                ## trying to fix the problem of having years
                        pass
                    else:
                        continue
                if (y,m) in seenDates:
                    # Output sum, as we will divide by sum of totals later
                    useCount = sum(counts[word][(y,m)])
                    ymLastUsed = (y,m)
                    filled = False
                else:
                    # If we didn't see this date in the capture, we want to use the last capture
                    if endOfMonthCounts[word][ymLastUsed]['date'] == lastCaptureOfMonth[ymLastUsed]:
                        useCount = endOfMonthCounts[word][ymLastUsed]['count']
                    else:
                        continue
			filled = True
                if useCount == 0:
                    continue
                outBag.append( (y, m, word, useCount, int(filled), int(afterLast), urls) )
                #outBag.append( (urlname, urlcount, y, m, word, useCount, int(filled), int(afterLast)) )
        return outBag
    except:
        pass


