package hod

import java.io.{File, FileInputStream, InputStream}
import java.text.DecimalFormat
import java.util.Properties
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.collection.{Iterator, mutable}
import scala.util.{Success, Try}

import org.apache.commons.codec.binary.Hex
import org.apache.commons.io.FileUtils
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClientBuilder
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.json4s.jackson.JsonMethods
import org.json4s.{DefaultFormats, JObject, JsonAST}

object Analyzer extends spells.Spells {

  implicit class StringOps(val str: String) extends AnyVal {
    def shiftRight(i: Int) = str.reverse.padTo(i, ' ').reverse

    def niceNumber = {
      Try {
        val value = str.toDouble
        if (value > 20) str.magenta
        if (value > 10) str.green
        else if (value > 0) str.black
        else str.red
      }.getOrElse(str)
    }
  }

  private val dateTimePatternFileName = "dd_MM_yyyy_HH_mm_ss"
  private val dateTimePatternCSV = "dd.MM.yyyy HH:mm:ss"
  private val timeLimitInMinutes = 1

  private val rootOfFiles = {
    List("/media/sf_vmsharedsync", "C:/Users/Test/Resilio Sync//vmsharedsync")
      .map(new File(_))
      .find(_.exists)
      .get
      .getAbsolutePath
  }
  private val authLocation = new File(s"$rootOfFiles/auth")
  private val props = {
    val prop = new Properties
    val stream = new FileInputStream(new File(s"$authLocation/key.txt"))
    prop.load(stream)
    prop
  }
  private val matrixStore = new File(s"$rootOfFiles/matrix")
  private val df = new DecimalFormat("#0.00")
  private val dfPrecise = new DecimalFormat("#0.000")
  private val dfSigned = new DecimalFormat("+#00.00;-#")
  private val key = props.getProperty("key")
  private val secret = props.getProperty("secret")

  private val host = "https://api.changelly.com"

  private val interestedIn = {
    Set("btc", "eth", "xmr",
      "dash", "xrp", "etc",
      "dcr", "nlg", "nav",
      "ltc", "doge", "pivx",
      "rads", "strat", "waves")
      .map(Currency)
  }

  def estimationFactor(currency: Currency) = {
    currency.code match {
      case "nlg" | "doge" => 10000
      case "xrp" => 250
      case x => 25
    }
  }

  case class Currency(code: String)

  case class Income(amount: Amount, when: DateTime)

  case class ExchangedAmount(original: Amount, switched: Amount, rate: Rate,
                             unprecise: Boolean = false) {
    def format = {
      val orgValue = df.format(original.value).shiftRight(10)
      val rateStr = dfPrecise.format(rate.factor).shiftRight(10) + (if (unprecise) "?" else " ")
      val rateTime = rate.recordedAt.toString(dateTimePatternCSV)
      s"$orgValue * $rateStr [$rateTime]"
    }

  }

  case class Amount(value: Double, currency: Currency) {
    def format = s"${dfPrecise.format(value)} ${currency.code}"

    def formatSpacey = s"${dfPrecise.format(value)} ${currency.code.shiftRight(5)}"
  }

  case class Rate(from: Currency, to: Currency, factor: Double, recordedAt: DateTime) {
    def asCsvRow = {
      List(from.code, to.code,
        factor,
        recordedAt.toString("dd.MM.yyy HH:mm:ss"))
        .mkString("\"", "\";\"", "\"")
    }
  }

  case class TransactionRecord(status: String, amountFrom: String, amountTo: String,
                               currencyFrom: String, currencyTo: String, createdAt: Long) {

    def isDone = status == "finished" || status == "sending"

    def typed = {
      val from = Amount(amountFrom.toDouble, Currency(currencyFrom))
      val to = Amount(amountTo.toDouble, Currency(currencyTo))
      val when = new DateTime(createdAt * 1000)
      Transaction(from, to, when)
    }
  }

  case class Transaction(given: Amount, gotten: Amount, when: DateTime) {
    def withFactor(ratio: Double) = {
      copy(given.copy(value = given.value * ratio), gotten.copy(value = gotten.value * ratio))
    }

    def toRate = Rate(given.currency, gotten.currency, gotten.value / given.value, when)

    def withMaxIncome(max: Double) = {
      val use = gotten.value min max
      if (use != gotten.value) {
        copy(gotten = gotten.copy(value = use))
      } else {
        this
      }
    }
  }

  case class ExchangeGuesstimationForWallet(balance: Amount, currency: Currency,
                                            options: List[ExchangeGuesstimation],
                                            originalCost: List[Amount]) {

    private def cost = {
      originalCost.map(_.format).mkString(" + ")
    }

    def format = {
      s"""|It is possible to exchange an amount of ${balance.format} (cost: $cost) for
          |${options.map(_.format).mkString("\n")}
       """.stripMargin
    }

    def bestOption = options.maxBy(_.sortBy)

  }

  case class ExchangeGuesstimation(ifExchangeIsDoneNow: Amount,
                                   explainAlt1: List[ExchangedAmount],
                                   explainAlt2: List[ExchangedAmount],
                                   ifHadNotDoneTheExchange: List[ExchangedAmount]) {

    private def amountExchangedIfHadKept = ifHadNotDoneTheExchange.map(_.switched).map(_.value).sum

    private def amountExchangedAlt1 = Amount(explainAlt1.map(_.switched).map(_.value).sum,
      explainAlt1.head.switched.currency)

    private def amountExchangedAlt2 = Amount(explainAlt2.map(_.switched).map(_.value).sum,
      explainAlt2.head.switched.currency)

    def format = {
      val now = dfPrecise.format(ifExchangeIsDoneNow.value).shiftRight(10)
      val describeNonAction = {
        val elements = {
          ifHadNotDoneTheExchange.map { e =>
            s"${e.original.format.shiftRight(12)} (=${e.switched.formatSpacey.shiftRight(17)})"
          }.mkString(" + ")
        }
        val sum = dfPrecise.format(amountExchangedIfHadKept).shiftRight(12)
        s"$sum ($elements)"
      }
      val percentToAlternativeSwitch = dfSigned.format((improvementRatioToSwitch - 1) * 100)
        .niceNumber
      val percentToAlternativeSwitch2 = dfSigned.format((improvementRatioToSwitch2 - 1) * 100)
        .niceNumber
      val percentToNonAction = dfSigned.format((improvementRatioToNonAction - 1) * 100).niceNumber
      val currency = ifExchangeIsDoneNow.currency.code.shiftRight(5)
      val details1 = amountExchangedAlt1.formatSpacey.shiftRight(17) + " = " +
        explainAlt1.map(_.format).mkString(" + ")
      val details2 = amountExchangedAlt2.formatSpacey.shiftRight(17) + " = " +
        explainAlt2.map(_.format).mkString(" + ")
      val altExchangeEarlier = s"$percentToAlternativeSwitch2% $details2"
      val details = s"$percentToAlternativeSwitch% $details1 || [alt exchange earlier] " + altExchangeEarlier
      val sharedDetails = details
      val altResult = amountExchangedAlt1.formatSpacey.shiftRight(17)
      val describeCompareToAction = s"$currency $now would be $sharedDetails"
      val describeCompareToNonAction = s"$currency $now would be [to unchanged] " +
        s"$percentToNonAction% -> $describeNonAction"
      if (improvementRatioToNonAction.isInfinity) {
        s"$currency $now compared to $altExchangeEarlier"
      } else {
        s"$describeCompareToNonAction || [if done earlier] $sharedDetails"
      }
    }

    val improvementRatioToSwitch = {
      ifExchangeIsDoneNow.value / amountExchangedAlt1.value
    }
    val improvementRatioToSwitch2 = {
      ifExchangeIsDoneNow.value / amountExchangedAlt2.value
    }
    val improvementRatioToNonAction = {
      ifExchangeIsDoneNow.value / amountExchangedIfHadKept
    }

    val sortBy = {
      List(improvementRatioToNonAction, improvementRatioToSwitch, improvementRatioToSwitch2).filterNot(_.isInfinity).sum
    }
  }

  case class RateMatrix(lastDate: DateTime, rates: List[Rate]) {
    def hasRatesFor(from: Currency, to: Currency): Boolean = {
      from == to || rates.exists(e => e.from == from && e.to == to)
    }

    def isValid = rates.forall(_.factor != 0.00)

    lazy val currencies = (rates.map(_.to) ++ rates.map(_.from)).distinct

    def estimateExchangeResult(amount: Amount, exchangeTo: Currency) = {
      if (amount.currency == exchangeTo || amount.currency.code == "magic") {
        ExchangedAmount(amount, amount.copy(currency = exchangeTo),
          Rate(amount.currency, exchangeTo, 1, DateTime.now))
      } else {
        val exchangeRate = rates.find(e => e.from == amount.currency && e.to == exchangeTo)

        exchangeRate.map { e =>
          val targetAmount = Amount(e.factor * amount.value, exchangeTo)
          ExchangedAmount(amount, targetAmount, e)
        }.getOrElse {
          // unknown = gets a bad rating
          ExchangedAmount(amount, Amount(0, exchangeTo),
            Rate(amount.currency, exchangeTo, 0, DateTime.now))
        }
      }
    }

    // just for fun see if switching to times can generate money out of nowhere

    def findBestDoubleExchange = {
      val currencies = rates.map(_.from).distinct
      currencies.flatMap { from =>
        rates.filter(_.from == from).flatMap { over =>
          val backToStart = rates.filter(_.from == over.to).filter(_.to == from)
          backToStart.map { to =>
            (from, over, to, 1 * over.factor * to.factor)
          }
        }
      }.maxBy(_._4)
    }

    def asCsv = {
      val contentRows = {
        rates.map { rate =>
          rate.asCsvRow
        }.mkString("\n")
      }
      "\"from\";\"to\";\"rate\";\"when\"\n" + contentRows
    }

    def storeAsNewFile() = {
      val fileName = s"matrix_${DateTime.now.toString(dateTimePatternFileName)}.csv"
      val target = new File(matrixStore, fileName)
      FileUtils.writeStringToFile(target, asCsv)
      println(s"Stored new data here: ${target.getAbsolutePath}")
    }
  }

  def doJsonPost[T: Manifest](method: String, params: (String, String)*) = {
    def consumeStream(in: InputStream) = {
      val bytes = {
        Iterator.continually(in.read())
          .takeWhile(_ != -1)
          .map(_.toByte)
          .toArray
      }
      in.close()
      bytes
    }

    val post = new HttpPost(host)
    val client = HttpClientBuilder.create().useSystemProperties().build()

    post.addHeader("api-key", key)
    post.addHeader("Content-type", "application/json")

    val values = {
      params.map {
        case (jKey, jValue) =>
          val jsonValue = {
            Try(jValue.toInt)
              .orElse(Try(jValue.toDouble))
              .orElse(Try(jValue)) match {
              case Success(i: Int) => JsonAST.JInt(i)
              case Success(d: Double) => JsonAST.JDouble(d)
              case Success(s: String) => JsonAST.JString(s)
              case _ => throw new IllegalStateException("crash")
            }
          }
          JsonAST.JField(jKey, jsonValue)
      }
    }

    val requestParams = JObject(
      JsonAST.JField("jsonrpc", JsonAST.JString("2.0")),
      JsonAST.JField("id", JsonAST.JInt(1)),
      JsonAST.JField("method", JsonAST.JString(method)),
      JsonAST.JField("params", JsonAST.JObject(values.toList))
    )

    val jsonPost = JsonMethods.compact(JsonMethods.render(requestParams))

    val sign = {
      val bytes = secret.getBytes("UTF-8")
      val secretKeySpec = new SecretKeySpec(bytes, "HmacSHA512")
      val mac = Mac.getInstance("HmacSHA512")
      mac.init(secretKeySpec)
      Hex.encodeHexString(mac.doFinal(jsonPost.getBytes("UTF-8")))
    }

    post.addHeader("sign", sign)

    post.setEntity(new StringEntity(jsonPost, "UTF-8"))
    println(s"OUT:$jsonPost")

    // send the post request
    val response = client.execute(post)
    val jsonResponse = {
      val stream = response.getEntity.getContent
      val bytes = consumeStream(stream)
      new String(bytes, "UTF-8")
    }

    println(s"IN:$jsonResponse")

    val parsed = JsonMethods.parse(jsonResponse)
    val result = parsed.asInstanceOf[JObject] \ "result"
    result.extract(DefaultFormats, manifest[T])
  }

  def getCurrencies = {
    doJsonPost[List[String]]("getCurrencies").map(Currency).toSet
  }

  def getExchangeRatio(from: Currency, to: Currency) = {
    val factor = estimationFactor(from)
    val ratio = doJsonPost[String]("getExchangeAmount", ("from", from.code), ("to", to.code),
      ("amount", factor.toString))
    Rate(from, to, (BigDecimal(ratio) / factor).toDouble, DateTime.now)
  }

  def buildMatrix(currencies: Set[Currency]) = {
    def tryGet = {
      println("Downloading exchange rates...")
      val sorted = currencies.toList.sortBy(_.code)
      val data = sorted.flatMap { from =>
        sorted.flatMap { to =>
          if (from != to) {
            Some(getExchangeRatio(from, to))
          } else {
            None
          }
        }
      }
      val latest = data.map(_.recordedAt).maxBy(_.getMillis)
      RateMatrix(latest, data)
    }

    Iterator.continually(tryGet).find(_.isValid).head
  }

  def determinePossibleExchanges(balance: List[Amount],
                                 transactionHistory: List[Transaction],
                                 checkAgainst: List[RateMatrix],
                                 isMined:Boolean):
  Seq[Analyzer.ExchangeGuesstimationForWallet] = {
    val earliest = checkAgainst.map(_.lastDate).minBy(_.getMillis)
    val currencies = {
      val relevant = balance.map(_.currency) ++ transactionHistory.map(_.gotten.currency)
      relevant.distinct.sortBy(_.code)
    }

    currencies.filter { c =>
      balance.exists(_.currency == c)
    }.map { focusOnCurrency =>
      val currentBalance = {
        balance.find(_.currency == focusOnCurrency)
          .getOrElse(Amount(0, focusOnCurrency))
      }

      def calcIncoming(cutOff: Boolean) = {
        val initialTransaction = {
          if (isMined) {
            balance.filter(_.currency == focusOnCurrency).map { e =>
              val fake = Amount(0, Currency("magic"))
              Transaction(fake, e, earliest)
            }
          } else {
            Nil
          }
        }

        val relevantTransactionsReversed = {
          if (isMined) {
            initialTransaction
          } else {
            val withFake = initialTransaction ++ transactionHistory
            withFake.filter { e =>
              e.gotten.currency == focusOnCurrency
            }.sortBy(_.when.getMillis).reverse
          }
        }

        var remainingBalanceToReach = currentBalance.value
        val transactionsUntilZero = mutable.ArrayBuffer.empty[Transaction]
        while (remainingBalanceToReach > 0.0001) {
          val nextTransaction = relevantTransactionsReversed.find { t =>
            !transactionsUntilZero.contains(t)
          }
          if (nextTransaction.isEmpty) {
            throw new IllegalStateException(
              s"could not find enough transactions to reach balance of $currentBalance using " +
                s"$relevantTransactionsReversed")
          }
          nextTransaction.foreach { add =>
            if (cutOff && remainingBalanceToReach < add.gotten.value) {
              val ratio = remainingBalanceToReach / add.gotten.value
              transactionsUntilZero += add.withFactor(ratio)
              remainingBalanceToReach -= add.gotten.value min remainingBalanceToReach

            } else {
              transactionsUntilZero += add
              remainingBalanceToReach -= add.gotten.value

            }
          }
        }
        transactionsUntilZero.toList.sortBy(_.when.getMillis)

      }

      val cost = {
        val targetAmount = currentBalance.value
        val pool = mutable.ArrayBuffer.empty ++= calcIncoming(false)
        val collected = mutable.ArrayBuffer.empty[Transaction]

        def collectedSum = collected.map(_.gotten.value).sum

        while (collectedSum < targetAmount) {
          if (pool.isEmpty) {
            throw new IllegalStateException("could not reconstruct payment")
          }

          val available = pool.last
          pool -= available
          val use = {
            val missing = targetAmount - collectedSum
            if (available.gotten.value > missing) {
              val factor = missing / available.gotten.value
              available.withFactor(factor)
            } else {
              available
            }
          }
          collected += use
        }
        collected.toList.map(_.given)
      }
      val incomingCutOff = calcIncoming(true).sortBy(_.when.getMillis)
      val options = determinePossibleExchangesForSingleWallet(incomingCutOff, checkAgainst)
      ExchangeGuesstimationForWallet(currentBalance, focusOnCurrency, options, cost)
    }.sortBy(_.bestOption.sortBy).reverse
  }

  def determinePossibleExchangesForSingleWallet(incomes: List[Transaction],
                                                against: List[RateMatrix]):
  List[ExchangeGuesstimation] = {
    assert(incomes.map(_.gotten.currency).distinct.size == 1)

    val walletCurrency = incomes.head.gotten.currency

    val targetCurrencies = against.head.currencies.filter(_ != walletCurrency)
    val mostRecent = against.maxBy(_.lastDate.getMillis)
    val totalBalance = {
      Amount(incomes.map(_.gotten.value).sum, incomes.head.gotten.currency)
    }
    targetCurrencies.map { targetCurrency =>
      def simulateExchange(getAmount: Transaction => Amount): List[ExchangedAmount] = {
        incomes.map { income =>
          if (income.gotten.currency == targetCurrency) {
            ExchangedAmount(income.given, income.gotten, income.toRate)
          } else {
            val withRates = against.filter(_.hasRatesFor(getAmount(income).currency, targetCurrency) ||
              getAmount(income).currency.code == "magic")
            if (withRates.isEmpty) throw new RuntimeException(s"Could not find matrix which has data from ${getAmount(income).currency} to $targetCurrency")
            val closestMatch = withRates.minBy { e =>
              (e.lastDate.getMillis - income.when.getMillis).abs
            }
            val day = 1000 * 60 * 60 * 24
            val beSuspicious = (closestMatch.lastDate.getMillis - income.when.getMillis).abs > day
            closestMatch.estimateExchangeResult(getAmount(income), targetCurrency)
              .copy(unprecise = beSuspicious)
          }
        }
      }

      val couldHaveGotten = {
        simulateExchange(_.given)
      }

      val couldHaveAlsoGotten = {
        simulateExchange(_.gotten)
      }

      val ifHadKept = incomes.map { e =>
        mostRecent.estimateExchangeResult(e.given, targetCurrency)
      }
      val ifExchangedNow = mostRecent.estimateExchangeResult(totalBalance, targetCurrency)
      ExchangeGuesstimation(ifExchangedNow.switched, couldHaveGotten, couldHaveAlsoGotten,
        ifHadKept)
    }.sortBy(_.sortBy).reverse
  }

  def loadData = {
    val historic = {
      matrixStore.getAbsoluteFile.listFiles().filter(_.getName.endsWith(".csv")).map { file =>
        val string = FileUtils.readFileToString(file)
        val rates = {
          string.lines.drop(1).toList.map { row =>
            val Array(from, to, rate, when) = row.split(';').map(_.drop(1).dropRight(1))
            val whenParsed = DateTime.parse(when, DateTimeFormat.forPattern(dateTimePatternCSV))
            Rate(Currency(from), Currency(to), rate.toDouble, whenParsed)
          }
        }
        RateMatrix(rates.map(_.recordedAt).maxBy(_.getMillis), rates)
      }.toList.sortBy(_.lastDate.getMillis)
    }
    println(s"found ${historic.size} old datasets")
    historic
  }

  def getTransactions = {
    val transactions = doJsonPost[List[TransactionRecord]]("getTransactions", ("limit", "9999"))
    val mapped = transactions.filter(_.isDone).map(_.typed)
    println(s"received $transactions transactions")
    mapped
  }

  def getHistory = {
    val currencies = getCurrencies
    val relevant = currencies.intersect(interestedIn)
    if (false && relevant != interestedIn) {
      throw new IllegalArgumentException {
        s"Missing: ${interestedIn.filterNot(currencies)}"
      }
    } else {
      val matrixHistory = loadData
      val dataIsOld = {
        matrixHistory.isEmpty ||
          matrixHistory.map(_.lastDate).maxBy(_.getMillis).plusMinutes(timeLimitInMinutes).isBeforeNow
      }
      val all = {
        if (dataIsOld) {
          val mostRecent = buildMatrix(relevant)
          mostRecent.storeAsNewFile()
          matrixHistory :+ mostRecent
        } else {
          matrixHistory
        }
      }

      all.filter(_.isValid)
    }
  }

  def buildBalance(now: (Double, String)*) = {
    now.map { case (amount, code) =>
      Amount(amount, Currency(code))
    }.toList
  }

  def main(args: Array[String]): Unit = {
    val transactions = getTransactions
    val history = getHistory
    val mined = buildBalance(
     // 0.48 -> "eth",
      (0.12+0.2) -> "dash",
    //  8.07 -> "etc",
    )

    val gottenViaTransactions = {
      buildBalance(
        113.0 -> "rads",
        17.765 -> "ltc",
        //6.61 -> "etc",
        3.83 -> "eth"
      ).groupBy(_.currency)
        .map { case (currency, amounts) =>
          Amount(amounts.map(_.value).sum, currency)
        }.toList
    }

    val allOptions = {
      determinePossibleExchanges(mined, transactions, history, isMined = true) ++
        determinePossibleExchanges(gottenViaTransactions, transactions, history, isMined = false)
    }
    allOptions.foreach { e =>
      val msg = e.format
      println(msg)
    }

    history.last.currencies.foreach { c =>
      val total = (gottenViaTransactions++mined).map { e => history.last.estimateExchangeResult(e, c) }
        .map(_.switched.value).sum
      println(
        s"Total balance in ${c.code.shiftRight(5)} = ${dfPrecise.format(total).shiftRight(12)}")
    }

  }
}
