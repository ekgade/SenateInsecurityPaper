from collections import defaultdict
import sys
import re
from string import punctuation

@outputSchema("URLs:chararray")
def pickURLs(url):
    try:
        names =set([
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

        regexp = re.compile ('([a-z]+)?(\.?\/?senate\.gov\/?\.?)([a-z]+)?')        
#        regexp = re.compile('([a-z]*)(\/([a-z]+))?')

        results = []

        result = regexp.search(url)
        if result is not None:
            if len(result.group(1)):
                if result.group(1) in names:
                    return(result.group(1))
            if len(result.group(3)):
# if result.group(2) is not None:
                if result.group(3) in names:
                    return(result.group(3))

    except:
        pass
    return 'other'


@outputSchema('counts:bag{tuple(word:chararray,count:int)}')
def Threat_countWords(content):
    try:
        Threat_Words = set([
                'accept',
                'accepta*',
                'accepted',
                'accepting',
                'accepts',
                'advantage',
                'adventur*',
                'assur*',
                'award*',
                'best',
                'bold',
                'brave*',
                'bright*',
                'certain*',
                'challeng*',
                'commitment*',
                'confidence*',
                'confidently',
                'confront*',
                'control',
                'convic*',
                'courag*',
                'daring',
                'definite*',
                'determina*',
                'determined',
                'ease*',
                'easy*',
                'efficien*',
                'encourag*',
                'enthus*',
                'excel*',
                'faith*',
                'flawless',
                'free*',
                'glorious',
                'glory',
                'hero*',
                'hope',
                'hoped',
                'hopef*',
                'hopes',
                'hoping',
                'impress*',
                'imporve*',
                'inspir',
                'optimi',
                'original',
                'pride',
                'profit*',
                'promising',
                'proud*',
                'ready',
                'secure',
                'securi*',
                'strong',
                'sunn*',
                'super',
                'superior*',
                'suprem*',
                'terrific*',
                'top',
                'triumph*',
                'trust*',
                'vigor*',
                'win',
                'winn*',
                'wins',
                'won',
                'god',
                'faith',
                'bless',
                'relig',
                'church',
                'pray',
                'amen',
                'crusade',
                'minister',
                'soul',
                'spiritual',
                'christ',
                'easter',
                'mass',
                'angel',
                'god',
                'hell',
                'preach',
                'evil',
                'churches',
                'catholic',
                'creed',
                'mirac',
                'creator',
                'lord',
                'heaven',
                'worship',
                'christian',
                'catholic',
                'almighty',
                'reverend',
                'christmas',
                'bible',
                'sacred',
                'pastor',
                'cardinal',
                'prophet',
                'baptist',
                'vigil',
                'congregation',
                'magi',
                'pope',
                'bishop',
                'sins',
                'protestant',
                'catholics',
                'preacher',
                'devil',
                'spiritually',
                'congregations',
                'evangelic',
                'evangelical',
                'hymn',
                'preaching',
                'covenant',
                'revelation',
                'salvation',
                'sermon',
                'divine',
                'evangelicals',
                'ministers',
                'holy',
                'saint',
                'christians',
                'scripture',
                'sin',
                'methodist',
                'clergy',
                'isaiah',
                'revelations',
                'fellowship',
                'quaker',
                'pastors',
                'biblical',
                'priest',
                'prophets',
                'pulpit',
                'believers',
                'commandment',
                'preached',
                'lord',
                'apostle',
                'godless',
                'monsignor',
                'godspeed',
                'baptists',
                'chapel',
                'kingdom',
                'cathedral',
                'parish',
                'archbishop',
                'christianity',
                'crusader',
                'exalted',
                'preaches',
                'altar',
                'denomination',
                'pew',
                'preachers',
                'proverb',
                'scriptures',
                'tabernacle',
                'eternity',
                'ministry',
                'monk',
                'sanctity',
                'conversions',
                'denominations',
                'jesus',
                'presbyterian',
                'saints',
                'apostles',
                'devout',
                'forsak',
                'puritan',
                'repent',
                'flock',
                'godsend',
                'lent',
                'priests',
                'redemption',
                'sanctuaries',
                'deacon',
                'jesuit',
                'savior',
                'archdiocese',
                'devine',
                'missionary',
                'mormon',
                'psalm',
                'pulpits',
                'theological',
                'exodus',
                'lutheran',
                'missionaries',
                'parishioner',
                'pews',
                'prayerfully',
                'prophecies',
                'sabbath',
                'samaritan',
                'sinner',
                'clergymen',
                'communion',
                'diocese',
                'gabriel',
                'minister',
                'monotheistic',
                'orthodox',
                'scriptural',
                'sect',
                'seminary',
                'unholy',
                'worshiper',
                'emmanuel',
                'episcopal',
                'fundamentalist',
                'holier',
                'martyrs',
                'messiah',
                'ministered',
                'ministries',
                'preacher',
                'prophesy',
                'psalmist',
                'psalms',
                'sacredness',
                'theology',
                'vatican',
                'angles',
                'baptized',
                'catholicism',
                'chaplain',
                'christened',
                'christening',
                'churchgoer',
                'clergyman',
                'congregants',
                'creator',
                'crucifix',
                'dios',
                'disciples',
                'ecclesiastes',
                'epistle',
                'evildoer',
                'genesis',
                'holiness',
                'moses',
                'nuns',
                'pastor',
                'pharaoh',
                'proselyt',
                'reverends',
                'theologian',
                'adventists',
                'angelic',
                'apostolic',
                'ark',
                'atonement',
                'baptism',
                'christ',
                'crucifixion',
                'evangelism',
                'exaltation',
                'galatians',
                'hallelujah',
                'micah',
                'monsignors',
                'nehemiah',
                'pope',
                'redeemer',
                'sanctif',
                'sects',
                'serpent',
                'steeple',
                'testaments',
                'ungodly',
                'vigils',
                'agape',
                'amish',
                'anglicans',
                'archabbey',
                'archabbot',
                'baptistries',
                'basilica',
                'beatitude',
                'believeth',
                'bible',
                'bibles',
                'calvinists',
                'canonized',
                'catholicos',
                'cbn',
                'christian',
                'churchgoing',
                'churchyard',
                'convent',
                'coreligionists',
                'corinthians',
                'deity',
                'deuteronomy',
                'diocesan',
                'dioceses',
                'easter',
                'emmanual',
                'ephesians',
                'espiritual',
                'evangelist',
                'exalteth',
                'ezekiel',
                'faithfuls',
                'fundamentalists',
                'godgiven',
                'godly',
                'halo',
                'heavenly',
                'herod',
                'holiest',
                'interreligious',
                'irreligious',
                'isaiah',
                'ishmael',
                'israelites',
                'lazarus',
                'lenten',
                'leviathan',
                'leviticus',
                'lucifer',
                'malachi',
                'mennonite',
                'messianic',
                'ministership',
                'monasteries',
                'monastery',
                'monks',
                'monotheism',
                'multisectarian',
                'navidad',
                'nazarenes',
                'nun',
                'papal',
                'pastorate',
                'pentecostal',
                'pharoah',
                'pieties',
                'pietists',
                'pontiff',
                'pontifical',
                'pontius',
                'popes',
                'priesthood',
                'prophesied',
                'prophesized',
                'prophesizing',
                'prophetess',
                'purgatories',
                'reverend',
                'rites',
                'rosaries',
                'sacrilegious',
                'satan',
                'satanic',
                'savior',
                'saviour',
                'sectarianism',
                'seminarian',
                'seminaries',
                'sinful',
                'sinned',
                'spirituality',
                'spirituals',
                'theocracy',
                'theocratic',
                'theologically',
                'vestments',
                'vicar',
                'zaccheus'])
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



#@outputSchema("counts:bag{tuple(year:int, month:int, word:chararray, count:int, filled:int, afterLast:int)}")
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




