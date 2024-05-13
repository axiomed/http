namespace Http.Data

--| Types for HTTP status codes.
inductive Status where
  | continued
  | switchingProtocols
  | processing
  | earlyHints
  | ok
  | created
  | accepted
  | nonAuthoritativeInformation
  | noContent
  | resetContent
  | partialContent
  | multiStatus
  | alreadyReported
  | imUsed
  | multipleChoices
  | movedPermanently
  | found
  | seeOther
  | notModified
  | useProxy
  | unused
  | temporaryRedirect
  | permanentRedirect
  | badRequest
  | unauthorized
  | paymentRequired
  | forbidden
  | notFound
  | methodNotAllowed
  | notAcceptable
  | proxyAuthenticationRequired
  | requestTimeout
  | conflict
  | gone
  | lengthRequired
  | preconditionFailed
  | payloadTooLarge
  | uriTooLong
  | unsupportedMediaType
  | rangeNotSatisfiable
  | expectationFailed
  | imATeapot
  | misdirectedRequest
  | unprocessableEntity
  | locked
  | failedDependency
  | tooEarly
  | upgradeRequired
  | preconditionRequired
  | tooManyRequests
  | requestHeaderFieldsTooLarge
  | unavailableForLegalReasons
  | internalServerError
  | notImplemented
  | badGateway
  | serviceUnavailable
  | gatewayTimeout
  | httpVersionNotSupported
  | variantAlsoNegotiates
  | insufficientStorage
  | loopDetected
  | notExtended
  | networkAuthenticationRequired
  deriving Repr

-- | Convert a Status to a numeric code. This is useful for sending the status code in a response.
def Status.toCode : Status -> Nat
  | continued                     => 100
  | switchingProtocols            => 101
  | processing                    => 102
  | earlyHints                    => 103
  | ok                            => 200
  | created                       => 201
  | accepted                      => 202
  | nonAuthoritativeInformation   => 203
  | noContent                     => 204
  | resetContent                  => 205
  | partialContent                => 206
  | multiStatus                   => 207
  | alreadyReported               => 208
  | imUsed                        => 226
  | multipleChoices               => 300
  | movedPermanently              => 301
  | found                         => 302
  | seeOther                      => 303
  | notModified                   => 304
  | useProxy                      => 305
  | unused                        => 306
  | temporaryRedirect             => 307
  | permanentRedirect             => 308
  | badRequest                    => 400
  | unauthorized                  => 401
  | paymentRequired               => 402
  | forbidden                     => 403
  | notFound                      => 404
  | methodNotAllowed              => 405
  | notAcceptable                 => 406
  | proxyAuthenticationRequired   => 407
  | requestTimeout                => 408
  | conflict                      => 409
  | gone                          => 410
  | lengthRequired                => 411
  | preconditionFailed            => 412
  | payloadTooLarge               => 413
  | uriTooLong                    => 414
  | unsupportedMediaType          => 415
  | rangeNotSatisfiable           => 416
  | expectationFailed             => 417
  | imATeapot                     => 418
  | misdirectedRequest            => 421
  | unprocessableEntity           => 422
  | locked                        => 423
  | failedDependency              => 424
  | tooEarly                      => 425
  | upgradeRequired               => 426
  | preconditionRequired          => 428
  | tooManyRequests               => 429
  | requestHeaderFieldsTooLarge   => 431
  | unavailableForLegalReasons    => 451
  | internalServerError           => 500
  | notImplemented                => 501
  | badGateway                    => 502
  | serviceUnavailable            => 503
  | gatewayTimeout                => 504
  | httpVersionNotSupported       => 505
  | variantAlsoNegotiates         => 506
  | insufficientStorage           => 507
  | loopDetected                  => 508
  | notExtended                   => 510
  | networkAuthenticationRequired => 511

def Status.fromCode : Nat → Option Status
  | 100 => Option.some continued
  | 101 => Option.some switchingProtocols
  | 102 => Option.some processing
  | 103 => Option.some earlyHints
  | 200 => Option.some ok
  | 201 => Option.some created
  | 202 => Option.some accepted
  | 203 => Option.some nonAuthoritativeInformation
  | 204 => Option.some noContent
  | 205 => Option.some resetContent
  | 206 => Option.some partialContent
  | 207 => Option.some multiStatus
  | 208 => Option.some alreadyReported
  | 226 => Option.some imUsed
  | 300 => Option.some multipleChoices
  | 301 => Option.some movedPermanently
  | 302 => Option.some found
  | 303 => Option.some seeOther
  | 304 => Option.some notModified
  | 305 => Option.some useProxy
  | 306 => Option.some unused
  | 307 => Option.some temporaryRedirect
  | 308 => Option.some permanentRedirect
  | 400 => Option.some badRequest
  | 401 => Option.some unauthorized
  | 402 => Option.some paymentRequired
  | 403 => Option.some forbidden
  | 404 => Option.some notFound
  | 405 => Option.some methodNotAllowed
  | 406 => Option.some notAcceptable
  | 407 => Option.some proxyAuthenticationRequired
  | 408 => Option.some requestTimeout
  | 409 => Option.some conflict
  | 410 => Option.some gone
  | 411 => Option.some lengthRequired
  | 412 => Option.some preconditionFailed
  | 413 => Option.some payloadTooLarge
  | 414 => Option.some uriTooLong
  | 415 => Option.some unsupportedMediaType
  | 416 => Option.some rangeNotSatisfiable
  | 417 => Option.some expectationFailed
  | 418 => Option.some imATeapot
  | 421 => Option.some misdirectedRequest
  | 422 => Option.some unprocessableEntity
  | 423 => Option.some locked
  | 424 => Option.some failedDependency
  | 425 => Option.some tooEarly
  | 426 => Option.some upgradeRequired
  | 428 => Option.some preconditionRequired
  | 429 => Option.some tooManyRequests
  | 431 => Option.some requestHeaderFieldsTooLarge
  | 451 => Option.some unavailableForLegalReasons
  | 500 => Option.some internalServerError
  | 501 => Option.some notImplemented
  | 502 => Option.some badGateway
  | 503 => Option.some serviceUnavailable
  | 504 => Option.some gatewayTimeout
  | 505 => Option.some httpVersionNotSupported
  | 506 => Option.some variantAlsoNegotiates
  | 507 => Option.some insufficientStorage
  | 508 => Option.some loopDetected
  | 510 => Option.some notExtended
  | 511 => Option.some networkAuthenticationRequired
  | _   => Option.none

def Status.text : Status → String
  | .continued => "Continue"
  | .switchingProtocols => "Switching Protocols"
  | .processing => "Processing"
  | .earlyHints => "Early Hints"
  | .ok => "OK"
  | .created => "Created"
  | .accepted => "Accepted"
  | .nonAuthoritativeInformation => "Non-Authoritative Information"
  | .noContent => "No Content"
  | .resetContent => "Reset Content"
  | .partialContent => "Partial Content"
  | .multiStatus => "Multi-Status"
  | .alreadyReported => "Already Reported"
  | .imUsed => "IM Used"
  | .multipleChoices => "Multiple Choices"
  | .movedPermanently => "Moved Permanently"
  | .found => "Found"
  | .seeOther => "See Other"
  | .notModified => "Not Modified"
  | .useProxy => "Use Proxy"
  | .unused => "Unused"
  | .temporaryRedirect => "Temporary Redirect"
  | .permanentRedirect => "Permanent Redirect"
  | .badRequest => "Bad Request"
  | .unauthorized => "Unauthorized"
  | .paymentRequired => "Payment Required"
  | .forbidden => "Forbidden"
  | .notFound => "Not Found"
  | .methodNotAllowed => "Method Not Allowed"
  | .notAcceptable => "Not Acceptable"
  | .proxyAuthenticationRequired => "Proxy Authentication Required"
  | .requestTimeout => "Request Timeout"
  | .conflict => "Conflict"
  | .gone => "Gone"
  | .lengthRequired => "Length Required"
  | .preconditionFailed => "Precondition Failed"
  | .payloadTooLarge => "Request Entity Too Large"
  | .uriTooLong => "Request URI Too Long"
  | .unsupportedMediaType => "Unsupported Media Type"
  | .rangeNotSatisfiable => "Requested Range Not Satisfiable"
  | .expectationFailed => "Expectation Failed"
  | .imATeapot => "I'm a teapot"
  | .misdirectedRequest => "Misdirected Request"
  | .unprocessableEntity => "Unprocessable Entity"
  | .locked => "Locked"
  | .failedDependency => "Failed Dependency"
  | .tooEarly => "Too Early"
  | .upgradeRequired => "Upgrade Required"
  | .preconditionRequired => "Precondition Required"
  | .tooManyRequests => "Too Many Requests"
  | .requestHeaderFieldsTooLarge => "Request Header Fields Too Large"
  | .unavailableForLegalReasons => "Unavailable For Legal Reasons"
  | .internalServerError => "Internal Server Error"
  | .notImplemented => "Not Implemented"
  | .badGateway => "Bad Gateway"
  | .serviceUnavailable => "Service Unavailable"
  | .gatewayTimeout => "Gateway Timeout"
  | .httpVersionNotSupported => "HTTP Version Not Supported"
  | .variantAlsoNegotiates => "Variant Also Negotiates"
  | .insufficientStorage => "Insufficient Storage"
  | .loopDetected => "Loop Detected"
  | .notExtended => "Not Extended"
  | .networkAuthenticationRequired => "Network Authentication Required"
