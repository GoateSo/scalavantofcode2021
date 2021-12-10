object Day3:
	// pt1 sol to be recreated
	def run2(lines : IndexedSeq[String]) =
		import scala.collection.mutable.HashMap
		var as = lines // o2
		var bs = lines // co2
		for i <- 0 until 12 do
			// find frequencies at char i
			var m1 = Array(0,0)
			var m2 = Array(0,0)
			for line <- as do m1(line(i)-'0') += 1 // o2 map
			for line <- bs do m2(line(i)-'0') += 1 // co2 map

			// find most / least common
			var m1c = if m1(0) > m1(1) then '0' else '1'
			// least common
			var m2c = if m2(0) <= m2(1) then '0' else '1'

			println((m1, m2))
			println((m1c, m2c))
			var a = as.filter(x => x(i) == m1c)
			var b = bs.filter(x => x(i) == m2c)
			if a.size > 0 then as = a
			if b.size > 0 then bs = b
		println((as, bs))