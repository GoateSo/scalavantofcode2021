class Day3(lines : IndexedSeq[String]):
  import Utils.toInt
  // pt1 sol to be recreated
  def run = 
    var cs = (0 until 12).map {i => 
      if lines.map(_(i)).count(_=='0') > 500
        then 1
        else 0 
    }.mkString
    var cs$ = cs.map(Map('1'->'0','0'->'1'))
    
    cs.toInt(2) * cs$.toInt(2)
    
  def run2 =
    var as, bs = lines // o2, co2
    // find frequencies at char i
    for i <- 0 until 12 do
      var m1,m2 = Array(0,0)
      for line <- as do 
        m1(line(i)-'0') += 1 // o2 map
      for line <- bs do 
        m2(line(i)-'0') += 1 // co2 map
      // most common
      var m1c = if m1(0) > m1(1) then '0' else '1'
      // least common
      var m2c = if m2(0) <= m2(1) then '0' else '1'

      val a = as.filter(_(i) == m1c)
      if a.size > 0 then as = a

      val b = bs.filter(_(i) == m2c)
      if b.size > 0 then bs = b
    as.head.toInt(2) * bs.head.toInt(2)