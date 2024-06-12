import Time

namespace Http.Util.Date

def RFC822 : Time.Format :=
  Time.Format.spec! "EEE, DD MMM YYYY hh:mm:ss 'GMT'"

def RFC850 : Time.Format :=
  Time.Format.spec! "EEEE, DD-MMM-YY hh:mm:ss 'GMT'"

def AscTime : Time.Format :=
  Time.Format.spec! "EEE MMM d hh:mm:ss YYYY"
