from collections import defaultdict
import sys
import re
from string import punctuation

@outputSchema("URLs:chararray")
def pickURLs(url):
    try:
        names =set([
            'dingell',
            'conyers',
            'rangel',
            'donyoung',
            'georgemiller',
            'waxman',
            'rahall',
            'lungren',
            'sensenbrenner',
            'petri',
            'dreier',
            'halrogers',
            'chrissmith',
            'wolf',
            'hoyer',
            'ackerman',
            'burton',
            'cooper',
            'kaptur',
            'levin',
            'joebarton',
            'coble',
            'visclosky',
            'defazio',
            'johnlewis',
            'price',
            'louise',
            'lamarsmith',
            'upton',
            'pelosi',
            'pallone',
            'duncan',
            'engel',
            'lowey',
            'mcdermott',
            'neal',
            'rohrabacher',
            'ros-lethitnen',
            'flake',
            'honda',
            'schiff',
            'issa',
            'jeffmiller',
            'crenshaw',
            'mikepence',
            'lynch',
            'mikerogers',
            'mccollum',
            'akin',
            'graves',
            'israel',
            'shuster',
            'langevin',
            'joewilson',
            'culberson',
            'matheson',
            'forbes',
            'cantor',
            'larsen',
            'capito',
            'lacyclay',
            'bonner',
            'franks',
            'grijalva',
            'nunes',
            'lisasanchez',
            'mariodiazbalart',
            'gingrey',
            'davidscott',
            'steveking',
            'chandler',
            'michaud',
            'rupperberger',
            'vanhollen',
            'candicemiller',
            'mccotter',
            'kline',
            'garrett',
            'pearce',
            'timbishop',
            'butterfield',
            'turner',
            'timryan',
            'cole',
            'gerlach',
            'murphy',
            'blackburn',
            'hensarling',
            'randy',
            'burgess',
            'carter',
            'robbishop',
            'costa',
            'wassermanschultz',
            'tomprice',
            'westmoreland',
            'barrow',
            'lipinski',
            'geoffdavis',
            'boustany',
            'cleaver',
            'fortenberry',
            'higgins',
            'foxx',
            'mchenry',
            'fitzpatrick',
            'schwartz',
            'dent',
            'gohmert',
            'poe',
            'algreen',
            'mccaul',
            'conaway',
            'marchant',
            'cuellar',
            'mcmorris',
            'reichert',
            'gwenmoore',
            'matsui',
            'campbell',
            'sires',
            'giffords',
            'mcnerney',
            'kevinmccarthy',
            'lamborn',
            'perlmutter',
            'courtney',
            'murphy',
            'castor',
            'buchanan',
            'hankjohnson',
            'hirono',
            'roskam',
            'donnelly',
            'braley',
            'loebsack',
            'yarmuth',
            'sarbanes',
            'walberg',
            'walz',
            'ellison',
            'bachmann,
            'adriansmith',
            'heller',
            'clarke',
            'shuler',
            'jordan',
            'sutton',
            'altmire',
            'cohen',
            'welch',
            'richardson',
            'broun',
            'tsongas',
            'latta',
            'wittman',
            'carson',
            'bilirakis',
            'scalise',
            'speier',
            'donnaedwards',
            'mcclintock',
            'polis',
            'coffman',
            'himes',
            'posey',
            'rooney',
            'schock',
            'guthrie',
            'fleming',
            'cassody',
            'pingrey',
            'peters',
            'paulsen',
            'harper',
            'luetkemeyer',
            'lance',
            'heinrich',
            'lujan',
            'chrislee',
            'kissell',
            'austria',
            'fudge',
            'schrader',
            'thompson',
            'roe',
            'olson',
            'chaffetz',
            'connolly',
            'lummis',
            'cquigley',
            'chu',
            'owens',
            'garmendi',
            'teddeutch',
            'critz',
            'tomgraves',
            'hunter',
            'stutzman',
            'reed',
            'sewell',
            'gosar',
            'quayle',
            'schweikert',
            'crawford',
            'griffin',
            'womack',
            'tipton',
            'gardner',
            'johncarney',
            'southerland',
            'nugent',
            'webster',
            'dennisross',
            'wilson',
            'west',
            'adams',
            'rivera',
            'woodall',
            'austinscott',
            'hanabusa',
            'labrador',
            'walsh',
            'dold',
            'kinzinger',
            'hultgren',
            'schilling',
            'rokita',
            'bucshon',
            'young',
            'huelskamp',
            'yoder',
            'pompeo',
            'richmond',
            'landry',
            'harris',
            'keating',
            'benishek',
            'huizenga',
            'amash',
            'hansenclarker',
            'cravaack',
            'nunnelee',
            'palazzo',
            'hartzler',
            'long',
            'heck',
            'guinta',
            'runyan',
            'hayworth',
            'gibson',
            'hanna',
            'buerkle',
            'ellmers',
            'berg',
            'billjohnson',
            'stivers',
            'renacci',
            'gibbs',
            'lankford',
            'kelly',
            'meehan',
            'marino',
            'barletta',
            'cicilline',
            'scott',
            'jeffduncan',
            'gowdy',
            'mulvaney',
            'noem',
            'fleischmann',
            'desjarlais',
            'black',
            'fincher',
            'flores',
            'canseco',
            'farenthold',
            'rigell',
            'hurt',
            'herrerabeutler',
            'mckinley',
            'duffy',
            'ribble',
            'morgangriffith',
            'roby', 
            'brooks',
            'hochel',
            'hahn',
            'amodei',
            'bobturner',
            'bonamici',
            'barber',
            'pastor',
            'waters',
            'delauro',
            'camp',
            'olver',
            'collinpeterson',
            'andrews',
            'serrano',
            'johnboehner',
            'samjohnson',
            'moran',
            'bachus',
            'woolsey',
            'eshoo',
            'farr',
            'mckeon',
            'becerra',
            'roybal-allard',
            'royce',
            'calvert',
            'corrinebrown',
            'mica',
            'alceehastings',
            'kingston',
            'bishop',
            'rush',
            'gutierrez',
            'manzullo',
            'bartlett',
            'benniethompson',
            'peteking',
            'nadler',
            'velazquez',
            'maloney',
            'hinchey',
            'watt',
            'lucas',
            'holden',
            'lofgren',
            'latham',
            'whitfield',
            'bass',
            'lobiondo',
            'frelinghuysen',
            'jones',
            'chabot',
            'fattah',
            'doyle',
            'doggett',
            'thornberry',
            'jacksonlee',
            'hastings',
            'cummings',
            'blumenauer',
            'aderholt',
            'sherman',
            'lorettasanchez',
            'degette',
            'davis',
            'shimkus',
            'boswell',
            'mcgovern',
            'tierney',
            'emerson',
            'pascrell',
            'rothman',
            'carolynmccarthy',
            'mcintyre',
            'kucinich',
            'pitts',
            'sessions',
            'kevinbrady',
            'kaygranger',
            'hinojosa',
            'adamsith',
            'kind',
            'capps',
            'bono',
            'meeks',
            'brady',
            'lee',
            'mikethompson',
            'napolitano',
            'garymiller',
            'larson',
            'simpson',
            'schakowsky',
            'judybiggert',
            'capuano',
            'leeterry',
            'berkley',
            'holt',
            'crowley',
            'walden',
            'paulryan',
            'baldwin',
            'gonzalez',
            'curson',
            'delbene',
            'massie',
            'payne',
            'clyburn',
            'green', 
            'ebjohson',
            'bobbyscott',
            'goodlatte',
            'alexander',
            'raphhall',
            'young',
            'stark',
            'kildee',
            'dicks',
            'paul',
            'jerrylewis',
            'frank',
            'berman',
            'towns',
            'gallegly',
            'herger',
            'costello',
            'payne',
            'stearns',
            'ross',
            'timjohnson',
            'rehberg',
            'sullivan',
            'platts',
            'cardoza',
            'bradmiller',
            'mack',
            'carnahan',
            'boren',
            'schmidt',
            'harman',
            'filner',
            'bilbray',
            'latourette',
            'jackson',
            'reyes',
            'weiner',
            'inslee',
            'wu',
            'baca'])

        regexp = re.compile ('([a-z]+)?(\.?\/?house\.gov\/?\.?)([a-z]+)?')
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
                'won'])
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





