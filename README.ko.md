# 소개 및 특징

로또 당첨 번호와 내가 구매한 로또의 당첨 여부를 Emacs에서 간편하게 확인할 수 있는 모듈입니다. 이 모듈은 다음과 같은 특징을 가지고 있습니다.

 * 인터넷을 통해 로또 정보를 수집하는 방법을 하나 이상 지원합니다.<br />
 로또 당첨 번호 정보는 인터넷을 통해 수집하게 되는데, 기본으로 @[Lotto\_k][1] 계정에 게시되는 것과 동일한 정보를 사용하는 [lotto.kaisyu.com 사이트의 Web API][2]를 사용합니다. 그리고, 혹시라도 이 사이트에 문제가 생겼을 경우를 대비해 네이버, 다음, 네이트, 나눔로또 공식 홈페이지 등의 대체 방법도 제공합니다.

 * Hashtable을 활용한 local cache를 유지합니다.<br />
 이로 인해 한번 가져온 로또 정보는 추가 인터넷 연결 없이 매우 빠르게 그 결과를 확인할 수 있습니다. 이 local cache는 lotto-check 모듈이 로드되거나 평가될 때 파일로부터 자동으로 읽혀져 초기화 되고, Emacs가 종료될 때 자동으로 파일에 저장됩니다.

 * 간단하게 즉시 사용할 수 있는 interactive 함수를 제공합니다.<br />
 *M-x 함수명* 형식으로 사용할 수 있는 함수들을 제공합니다. 로또 당첨 번호 정보를 보여주고, 사용자가 제공한 번호의 당첨 여부도 확인해주는 함수들로, 그 결과가 새로운 전용 buffer에 출력되거나 message 형태로 보여집니다. 자세한 것은 아래에 다시 소개하겠습니다.

 * 다른 모듈에서 간편하게 사용할 수 있는 API 함수를 제공합니다.<br />
 위에서 소개한 interactive 함수들과 동일한 역할을 하는 API 함수가 존재하는데, interactive 함수들과 다른 점은 반환값이 lisp object 형태로 되어 있어 재가공하거나 변환하여 사용하기 편리하다는 점입니다.

----

# 설치 및 설정

## 설치

다운로드한 소스를 load path에 추가해줍니다.

```lisp
(add-to-list 'load-path "[lotto-check 모듈이 있는 경로]")
(require 'lotto-check)
```

## 설정

Emacs의 Customize 기능(**M-x customize**)을 사용해서 Applications 그룹 아래의 Lotto 그룹으로 가면 간편하게 설정할 수 있는 UI가 제공되지만, 아래와 같이 직접 startup script 파일에 설정을 추가하는 것도 가능합니다.

```lisp
;; 로또 당첨 정보를 제공하는 데이터 소스를 지정합니다.
;; 기본값은 +lotto-data-source-lotto-k+ 이며,
;; 네이버, 다음, 네이트, 나눔로또 공식 홈페이지, 사용자 정의 방법 등을 제공합니다.
;; 네이버: +lotto-data-source-naver+
;; 다음: +lotto-data-source-daum+
;; 네이트: +lotto-data-source-nate+
;; 나눔로또 공식 홈페이지: +lotto-data-source-645lotto+
;; 사용자 정의 소스: lotto-info-data-source-custom
;; * 사용자 정의 소스 방식을 사용할 경우 lotto-info-data-source-custom 변수에
;;   사용자 정의 소스를 지정합니다.
;;   예) (setq lotto-info-data-source-custom 'my-lotto-info-data-src-1)
(setq lotto-info-data-source +lotto-data-source-lotto-k+)
;; 로또 당첨 정보를 저장할 파일을 지정합니다.
(setq lotto-database-file "~/.lotto-database")
;; interactive function 실행 시 결과를 별도의 buffer에 보여주도록 설정합니다.
(setq lotto-use-buffer-for-message t)
```

----

# 사용

## interactive 함수 사용하기

 * **M-x lotto-start** <br/>
 로또 모드를 시작합니다. 로또 메시지를 위한 버퍼가 만들어지고 포커스가 이동되며 이 버퍼에서는 몇 가지 간단한 단축키가 지원됩니다. 이 단축키들은 아래에 나열된 개별 command에 각각 대응됩니다.
  * **h** - 도움말을 보여줍니다.
  * **g** - 또는 **r** 로또 당첨 번호 정보를 보여줍니다.
  * **c** - 특정 회차에 사용자가 지정한 번호의 당첨 여부를 확인합니다.
  * **l** - 로또 당첨 번호 정보를 불러옵니다.
  * **s** - 로또 당첨 번호 정보를 저장합니다.
  * **!** - 로또 모드 버퍼의 내용을 초기화합니다.
  * **q** - 로또 모드 버퍼를 숨깁니다.
  * **Q** - 로또 모드 버퍼를 영구히 종료합니다.
 * **M-x lotto-display-help-message** <br />
 도움말을 보여줍니다.
 * **M-x lotto-retrieve-numbers-i** <br />
 특정 회차의 당첨 번호 정보를 보여줍니다. 실행하면 mini-buffer에 **game no:** 프롬프트가 뜨는데, 가져올 로또 회차 번호를 입력합니다.
  예) game no: **397**
 * **M-x lotto-check-numbers-list-i** <br />
 특정 회차에 사용자가 지정한 번호의 당첨 여부를 확인하여 결과를 보여줍니다. 실행하면 minu-buffer에 **game no:** 프롬프트와 **your numbers:** 프롬프트가 뜨는데, 각각 비교할 로또 회차 번호와 확인할 번호 list를 입력합니다. 확인할 번호 list 형식은 일반적인 lisp object와 동일합니다.
  예) your numbers: **((1 14 16 25 33 42) (2 10 17 26 34 43))**
 * **M-x lotto-save-db-to-file-i** <br />
 local cache에 저장된 로또 당첨 번호 정보를 파일에 저장합니다. 사용자가 수동으로 저장하지 않아도 Emacs 종료 시 자동으로 저장됩니다.
 * **M-x lotto-load-db-to-file-i** <br />
 파일에 저장된 로또 당첨 번호 정보를 불러옵니다. 사용자가 수동으로 불러오지 않아도 lotto-check 모듈을 불러올 때 자동으로 local cache도 함께 불러옵니다.
 * **M-x lotto-clear-message-buffer** <br />
 로또 모드 버퍼의 내용을 초기화합니다.
 * **M-x lotto-hide-message-buffer** <br />
 로또 모드 버퍼를 숨깁니다.
 * **M-x lotto-kill-message-buffer** <br />
 로또 모드 버퍼를 영구히 종료합니다.

## lotto-check 모듈의 interactive 함수들 실행 결과

 ![lotto-check 실행 결과](http://1.bp.blogspot.com/-9oAUkzimNXk/Ta0-IrsecpI/AAAAAAAAAmw/m4OrGHDqTBo/s1600/lotto-check-el-2.png)

## API 함수 사용하기

```lisp
;; (lotto-retrieve-numbers GNO)
;;   로또 당첨 번호를 반환합니다.
;;   GNO: 가져올 로또 회차 번호
;;   반환값: 로또 당첨 번호 정보를 포함한 alist 객체

(lotto-retrieve-numbers 395)
=> ((bnum . 7)
    (gno . 395)
    (gdate . "2010-06-26")
    (nums 11 15 20 26 31 35))


;; (lotto-check-numbers-list GNO MY-NUM-LIST)
;;   주어진 번호의 당첨 여부를 반환합니다.
;;   GNO: 비교할 로또 회차 번호
;;   MY-NUM-LIST: 확인할 번호들의 list
;;   반환값: 등수와 일치한 번호를 포함한 alist 객체의 list

(lotto-check-numbers-list 395 '((1 2 3 4 5 6) (11 15 20 28 32 36)))
=> (((rank . 0)
     (matched . nil))
    ((rank . 5)
     (matched . (11 15 20))))
```


[1]: http://twitter.com/lotto_k
[2]: http://blog.kaisyu.com/2010/07/web-api.html
