import  urllib2
import  numpy as np
import  matplotlib.pyplot as plt
from    pandas import *
from    datetime import *
import  scipy.stats
import  math

import  re
import  os

class MarkowitzPortfolio(object):
    def __init__(self, dateBegin, dateEnd, timeMarker, stockNames, weights, initialWealth):
        
        if not os.path.exists('data'):
            os.makedirs('data')
            
        self.dateBegin  =dateBegin
        self.dateEnd    =dateEnd
        self.timeMarker =timeMarker
        if(dateEnd=='today'):
            dateEnd     =str(datetime.today())
            dateEnd     =dateEnd.split(" ")
            dateEnd     =dateEnd[0]
            dateEnd     =dateEnd.split("-")
            temp        =dateEnd[2]
            dateEnd[2]  =dateEnd[0]
            dateEnd[0]  =dateEnd[1]
            dateEnd[1]  =temp
            dateEnd='/'.join(dateEnd)

        self.stockNames     = stockNames
        self.weights        = np.array(weights,dtype=np.float64)
        self.n              = self.weights.size
        self.initialWealth  = np.array(initialWealth,dtype=np.float64)

        dateBeginFileFormat = dateBegin.replace("/","-")
        dateEndFileFormat   = dateEnd.replace("/","-")
        stockFiles          = []
        
        N                   = len(stockNames) #number of assets
        print               "\t%d symbols" % N
        
        for stock in stockNames: #download the data for each single stock
            print "\tfetching", stock
            self.downloadData(dateBegin, dateEnd, stock, timeMarker)
            stockFiles.append(stock+"-"+dateBeginFileFormat+"-to-"+dateEndFileFormat+".csv")
            
        firstStock          = stockFiles[0]
        data                = read_csv('data/'+firstStock, na_values=[" "])
        close               = np.array(data["Close"],dtype=np.float64)
        T                   = close.size
        Matrix              = DataFrame(np.zeros([T, N]), columns=stockNames)
        Matrix[stockNames[0]] = close
        for k in range(1,N):
            stock           = stockFiles[k]
            data            = read_csv('data/'+stock, na_values=[" "])
            close           = np.array(data["Close"],dtype=np.float64)
            Matrix[stockNames[k]]=close
        self.Matrix=Matrix

        present=np.array(Matrix[:-1],dtype=np.float64)
        future=np.array(Matrix[1:],dtype=np.float64)
        Returns=(future/present)-1
        Returns=DataFrame(Returns, columns=stockNames)
        self.Returns=Returns
        self.meanReturns=Returns.mean()
        self.varianceCovarianceReturns=Returns.cov()
        self.expectedReturn=np.dot(self.weights,self.meanReturns)
        varianceTemp=np.dot(self.weights,self.varianceCovarianceReturns)
        self.variance=np.dot(varianceTemp,self.weights)


        covarianceMatrix=np.array(self.varianceCovarianceReturns)
        meansMatrix=np.array(self.meanReturns).reshape(self.n,1)
        onesMatrix=np.ones([self.n,1])
        oneSingle=np.ones([1,1])
        zerosMatrix=np.zeros([self.n,1])
        zeroSingle=np.zeros([1,1])

        A1=np.concatenate((2*covarianceMatrix, onesMatrix), axis=1)
        A2=np.concatenate((onesMatrix.T,zeroSingle), axis=1)
        A=np.concatenate((A1,A2), axis=0)
        b=np.concatenate((zerosMatrix, oneSingle), axis=0)
        z=np.dot(np.linalg.inv(A),b)
        x=z[:self.n]
        self.globalMinimumVarianceNp=x
        xMean=np.dot(x.T,self.meanReturns)
        xVarTemp=np.dot(x.T,self.varianceCovarianceReturns)
        xVar=np.dot(xVarTemp,x)
        x=np.append(x, [xMean, xVar])
        legend=stockNames[:]
        legend.append("Expected Return")
        legend.append("Variance")
        self.legend=legend
        self.globalMinimumVariance=DataFrame(x.reshape(1,(self.n)+2), columns=legend)


        targetReturn=np.array(self.expectedReturn).reshape(1,1)
        self.minVarianceWeights=self.minVariance(targetReturn)
        minVarianceMean=np.dot(self.minVarianceWeights.T,self.meanReturns)
        minVarianceVarTemp=np.dot(self.minVarianceWeights.T,self.varianceCovarianceReturns)
        minVarianceVar=np.dot(minVarianceVarTemp,self.minVarianceWeights)
        temp=np.append(self.minVarianceWeights.reshape(1,self.n),minVarianceMean)
        temp=np.append(temp,minVarianceVar)
        self.minimumVarianceWeights=DataFrame(temp.reshape(1,(self.n)+2), columns=legend)
        temp=np.array(self.weights)
        temp=np.append(temp,self.expectedReturn)
        temp=np.append(temp,self.variance)
        self.estimate=DataFrame(temp.reshape(1,(self.n)+2), columns=legend)
        self.initialWealth=initialWealth

    def downloadData(self,dateBegin,dateEnd,stockName,timeMarker): 
        #format pour les dates: "mois/jour/annee" stockName ex : "MSFT" pour microsoft
        #dans l'url a et e representent les mois et vont de [00,...,11] (00 pour janvier et 11 pour decembre)
        #b et e representent les jours
        #c et f representent les annees
        dateBeginTab=dateBegin.split("/")
        dateEndTab=dateEnd.split("/")
        dateBeginTab[0]=str(int(dateBeginTab[0])-1)
        dateEndTab[0]=str(int(dateEndTab[0])-1)
        if (timeMarker=='monthly'):
            timeMarker='m'
        else:
            timeMarker='d'
        dataFile = urllib2.urlopen("http://ichart.finance.yahoo.com/table.csv?s="+stockName+"&a="+dateBeginTab[0]+"&b="+dateBeginTab[1]+"&c="+dateBeginTab[2]+"&d="+dateEndTab[0]+"&e="+dateEndTab[1]+"&f="+dateEndTab[2]+"&g="+timeMarker+"&ignore=.csv")
        temp=dataFile.read()
        temp=temp.split("\n")
        header=temp.pop(0)
        temp.reverse() #range par ordre chronologique
        temp.pop(0) #car apres avoir reverse() le premier element est un element vide
        temp.insert(0,header)
        temp="\n".join(temp)
        savingFileName=stockName+'-'+dateBegin.replace("/","-")+'-to-'+dateEnd.replace("/","-")+'.csv'
        output = open('data/'+savingFileName,'wb')		
        output.write(temp)
        output.close()
        dataFile.close()
        return temp

    def minVariance(self,targetReturn): #weights for the minimum variance for a target return
        targetReturn            =np.array(targetReturn).reshape(1,1)
        covarianceMatrix        =np.array(self.varianceCovarianceReturns)
        meansMatrix             =np.array(self.meanReturns).reshape(self.n,1)
        onesMatrix              =np.ones([self.n,1])
        oneSingle               =np.ones([1,1])
        zerosMatrix             =np.zeros([self.n,1])
        zeroSingle              =np.zeros([1,1])
        A1                      =np.concatenate((2*covarianceMatrix, meansMatrix, onesMatrix), axis=1)
        A2                      =np.concatenate((meansMatrix.T,zeroSingle,zeroSingle), axis=1)
        A3                      =np.concatenate((onesMatrix.T,zeroSingle,zeroSingle), axis=1)
        A                       =np.concatenate((A1,A2,A3), axis=0)
        b                       =np.concatenate((zerosMatrix,targetReturn,oneSingle), axis=0)
        z                       =np.dot(np.linalg.inv(A),b)
        return                  z[:self.n]

    def ComputeMinVariance(self,targetReturn):
        x                       =self.minVariance(targetReturn)
        xMean                   =np.dot(x.T,self.meanReturns)
        xVarTemp                =np.dot(x.T,self.varianceCovarianceReturns)
        xVar                    =np.dot(xVarTemp,x)
        x                       =np.append(x, [xMean, xVar])
        print DataFrame(x.reshape(1,(self.n)+2), columns=self.legend)
        return                  x

    def efficientFrontier(self):
        x=self.globalMinimumVarianceNp
        xMean=np.dot(x.T,self.meanReturns)
        xVarTemp=np.dot(x.T,self.varianceCovarianceReturns)
        xVar=np.dot(xVarTemp,x)
        yMean=np.array((self.meanReturns).max()).reshape(1,1)
        y=self.minVariance(yMean)
        yVarTemp=np.dot(y.T,self.varianceCovarianceReturns)
        yVar=np.dot(yVarTemp,y)
        xVary=np.dot(xVarTemp,y)
        a=np.linspace(-1,1,100)
        zMean=a*xMean+(1-a)*yMean
        zVar=np.power(a,2)*xVar+np.power(1-a,2)*yVar+2*a*(1-a)*xVary
        return zVar, zMean

    def plot(self):
        var, mean       =self.efficientFrontier()
        axScatter       = plt.subplot(111)
        axScatter.scatter(var, mean,s=1,label='Efficient Frontier')
        axScatter.scatter(self.variance, self.expectedReturn, marker='*', s=40, color='red',label='Your Portfolio')
        bestMean        =np.dot(self.minVarianceWeights.T,self.meanReturns)
        bestVarTemp     =np.dot(self.minVarianceWeights.T,self.varianceCovarianceReturns)
        bestVar         =np.dot(bestVarTemp,self.minVarianceWeights)
        axScatter.scatter(bestVar, bestMean, marker='*', s=40,color='green',label='Best Portfolio')
        axScatter.set_xlabel("Risk (Variance)",fontsize=20)
        axScatter.set_ylabel("Expected Return",fontsize=20)
        xmin            =min(0,var.min(),bestVar,self.variance)
        xmax            =max(var.max(),bestVar,self.variance)
        ymin            =min(mean.min(),bestMean,self.expectedReturn)
        ymax            =max(mean.max(),bestMean,self.expectedReturn)
        axScatter.set_xlim([xmin, xmax])
        axScatter.set_ylim([ymin, ymax])
        axScatter.grid(True,linestyle='-')
        axScatter.legend(numpoints=1, loc='upper left')
        axScatter.set_title("Markowitz Representation",fontsize=20)
        plt.show()

	def VaR(self,a):
		return (self.expectedReturn + math.sqrt(self.variance)*scipy.stats.norm.ppf(a))*self.initialWealth

	def sourceCode(self,url):
		usock = urllib2.urlopen(url)
		data = usock.read()
		usock.close()
		return data
	
	def fetchSymbols(self): #fetch all the NYSE symbols
		print "Please wait while the symbols list is being downloaded from Yahoo..."
		symbols=list()
		link="http://finance.yahoo.com/q/cp?s=^NYA&c="
		page=self.sourceCode('http://finance.yahoo.com/q/cp?s=^NYA')
		temp2=re.findall('Next</a> '+'\S+'+' '+'<a href="'+'\S+'+'>Last</a>',page)
		temp2=temp2[0]
		marker=temp2.find("c=")
		temp2=temp2[marker+2:]
		numberOfPages=re.findall('\S+'+'"',temp2)
		numberOfPages=numberOfPages[0]
		numberOfPages=numberOfPages[:-1]
		numberOfPages=int(numberOfPages)
		total=page
		if (numberOfPages>1):
			for k in range(2,numberOfPages+1):
				total=total+self.sourceCode(link+str(k))
		temp1=re.findall('<b><a href="/q'+'\S+'+'">',total)
		for singleSymbol in temp1:
			symbols.append(singleSymbol[17:-2])
		return symbols

	def findNextPortfolioComponent(self,targetReturn):
		symbols=self.fetchSymbols()
		first=symbols[0]
		stockNamesTemp=self.stockNames[:]
		stockNamesTemp.append(first)
		weightsTemp=self.weights[:]
		weightsTemp=np.append(weightsTemp,1)
		portfolioTemp=MarkowitzPortfolio(self.dateBegin, self.dateEnd, self.timeMarker, stockNamesTemp,weightsTemp,self.initialWealth)
		temp=portfolioTemp.ComputeMinVariance(targetReturn)
		best=0
		lowestVariance=temp[(portfolioTemp.n)+1]
		for k in range(1,len(symbols)):
			newStock=symbols[k]
			stockNamesTemp=self.stockNames[:]
			stockNamesTemp.append(newStock)
			weightsTemp=self.weights[:]
			weightsTemp=np.append(weightsTemp,1)
			capt=1
			try:
				portfolioTemp=MarkowitzPortfolio(self.dateBegin, self.dateEnd, self.timeMarker, stockNamesTemp,weightsTemp,self.initialWealth)
			except:
				capt=0
			if(capt==1):
				temp=portfolioTemp.ComputeMinVariance(targetReturn)
				if(temp[(portfolioTemp.n)+1]<lowestVariance):
					lowestVariance=temp[(portfolioTemp.n)+1]
					best=k
		stockNamesTemp=self.stockNames[:]
		stockNamesTemp.append(symbols[best])
		weightsTemp=self.weights[:]
		weightsTemp=np.append(weightsTemp,1)
		portfolioTemp=MarkowitzPortfolio(self.dateBegin, self.dateEnd, self.timeMarker, stockNamesTemp,weightsTemp,self.initialWealth)
		print "Best new portfolio's component: "+symbols[best]
		return portfolioTemp.ComputeMinVariance(targetReturn)

    
if __name__ == "__main__":
    print "\tok so far"
    stocknames  = ["ANN", "ARMH", "BMO", "BOH", "CNDA", "DVY", "EAT",
        "ENY", "EWC", "EWS", "HE", "HNR", "IYR", "JUNR", "JWN", "LQD", 
        "POT", "TNH"]
        
    weights     = [.091, .131, .0336, .0622, .0413, .0415, .0472, .0444, 
        .0616, .045, .0652, .0161, .0366, .0184, .0651, .1146, .028, .0574]
    
    print "\tsum  of weights = %6.3f" % sum(weights)
    
    myPortfolio = MarkowitzPortfolio("01/01/2012", "today", "daily", 
        stocknames, weights, 100000)
        
    print "\n\tmean returns"
    print myPortfolio.meanReturns
        
    print
    print myPortfolio.varianceCovarianceReturns
    print
    
    
    print myPortfolio.minimumVarianceWeights
    print
    
    print
    myPortfolio.ComputeMinVariance(0.10)
    
    print
    myPortfolio.ComputeMinVariance(0.15)
    
    print
    myPortfolio.ComputeMinVariance(0.20)
    
    myPortfolio.plot()
    
