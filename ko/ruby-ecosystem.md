---
category: tool
name: Ruby 생태계
contributors:
    - ["Jon Smock", "http://github.com/jonsmock"]
    - ["Rafal Chmiel", "http://github.com/rafalchmiel"]

---

Ruby를 사용하는 사람들은 일반적으로 다른 Ruby 버전을 설치하고,
패키지(또는 젬)를 관리하고, 젬 종속성을 관리하는 방법을 가지고 있습니다.

## Ruby 버전

Ruby는 마츠모토 유키히로("Matz")가 만들었으며, 그는 최근에 바뀌고 있지만 다소
[BDFL](https://en.wikipedia.org/wiki/Benevolent_Dictator_for_Life)로 남아 있습니다.
결과적으로 Ruby의 참조 구현은
MRI(Matz' Reference Implementation)라고 하며, Ruby 버전을 들으면
MRI의 릴리스 버전을 의미합니다.

새로운 주요 Ruby 버전은 전통적으로 크리스마스에 출시됩니다. 현재 주요 버전(2017년 12월 25일)은 2.5입니다. 가장 인기 있는 안정 버전은 2.4.4와 2.3.7입니다(둘 다 2018년 3월 28일 출시).

## Ruby 관리자

일부 플랫폼에는 Ruby가 사전 설치되어 있거나 패키지로 제공됩니다. 대부분의 루비스트는
이것들을 사용하지 않거나, 사용하더라도 다른 Ruby
설치 프로그램이나 구현을 부트스트랩하는 데만 사용합니다. 대신 루비스트는 Ruby 관리자를
설치하여 여러 버전의 Ruby와 프로젝트의 Ruby
환경을 설치하고 전환하는 경향이 있습니다.

다음은 인기 있는 Ruby 환경 관리자입니다:

* [RVM](https://rvm.io/) - 루비를 설치하고 전환합니다. RVM은 또한
  프로젝트의 환경을 완전히 격리하기 위한 젬셋 개념을 가지고 있습니다.
* [ruby-build](https://github.com/sstephenson/ruby-build) - 루비만 설치합니다.
  루비 설치를 더 세밀하게 제어하려면 이것을 사용하십시오.
* [rbenv](https://github.com/sstephenson/rbenv) - 루비 간에만 전환합니다.
  ruby-build와 함께 사용됩니다. 루비 로드 방식을 더 세밀하게 제어하려면 이것을 사용하십시오.
* [chruby](https://github.com/postmodern/chruby) - 루비 간에만 전환합니다.
  정신적으로 rbenv와 유사합니다. 루비 설치 방식에 대해 의견이 없습니다.

## Ruby 구현

Ruby 생태계는 각각 고유한 강점과 호환성 상태를 가진
많은 다른 Ruby 구현을 즐깁니다. 명확히 하자면, 다른
구현은 다른 언어로 작성되었지만, *모두 Ruby*입니다.
각 구현에는 특별한 후크와 추가 기능이 있지만, 모두
정상적인 Ruby 파일을 잘 실행합니다. 예를 들어, JRuby는 Java로 작성되었지만,
사용하기 위해 Java를 알 필요는 없습니다.

매우 성숙/호환 가능:

* [MRI](https://github.com/ruby/ruby) - C로 작성된 Ruby의 참조 구현입니다.
  정의상 100% 호환됩니다(자체적으로). 다른 모든 루비는
  MRI와 호환성을 유지합니다(아래 [Ruby Spec](#ruby-spec) 참조).
* [JRuby](http://jruby.org/) - Java와 Ruby로 작성된 이 견고한 구현은 매우 빠릅니다.
  가장 중요한 것은 JRuby의 강점은 JVM/Java 상호 운용성이며, 기존
JVM 도구, 프로젝트 및 언어를 활용합니다.
* [Rubinius](http://rubini.us/) - 주로 Ruby 자체로 작성되었으며 C++ 바이트코드 VM이 있습니다. 또한
  성숙하고 빠릅니다. Ruby 자체로 구현되었기 때문에 많은 VM
기능을 루비랜드에 노출합니다.

중간 정도 성숙/호환 가능:

* [Maglev](http://maglev.github.io/) - Smalltalk VM인 Gemstone 위에 구축되었습니다. Smalltalk에는 몇 가지
  인상적인 도구가 있으며, 이 프로젝트는 그것을 Ruby
개발에 도입하려고 합니다.
* [RubyMotion](http://www.rubymotion.com/) - Ruby를 iOS 개발에 도입합니다.

덜 성숙/호환 가능:

* [Topaz](http://topazruby.com/) - RPython(PyPy 도구 체인 사용)으로 작성된 Topaz는 상당히 젊고
  아직 호환되지 않습니다. 고성능 Ruby
구현이 될 가능성을 보여줍니다.
* [IronRuby](http://ironruby.net/) - C#으로 작성되어 .NET 플랫폼을 대상으로 하는 IronRuby 작업은
  Microsoft가 지원을 철회한 이후 중단된 것으로 보입니다.

Ruby 구현은 자체 릴리스 버전 번호를 가질 수 있지만, 항상
호환성을 위해 특정 버전의 MRI를 대상으로 합니다. 많은 구현에는
어떤 MRI 버전을 대상으로 할지 지정하기 위해 다른 모드(예: 1.8 또는 1.9 모드)로
들어가는 기능이 있습니다.

## Ruby Spec

대부분의 Ruby 구현은 [Ruby Spec](https://github.com/ruby/spec)에 크게 의존합니다. Ruby
에는 공식 사양이 없으므로 커뮤니티는
Ruby로 실행 가능한 사양을 작성하여 구현의 MRI와의
호환성을 테스트합니다.

## RubyGems

[RubyGems](http://rubygems.org/)는 Ruby용 커뮤니티 운영 패키지 관리자입니다.
RubyGems는 Ruby와 함께 제공되므로 별도로 다운로드할 필요가 없습니다.

Ruby 패키지는 "젬"이라고 하며, 커뮤니티에서
RubyGems.org에 호스팅할 수 있습니다. 각 젬에는 소스 코드와 버전,
종속성, 작성자, 라이선스 등과 같은 일부 메타데이터가 포함되어 있습니다.

## Bundler

[Bundler](http://bundler.io/)는 젬 종속성 해결사입니다. 프로젝트의
Gemfile을 사용하여 종속성을 찾은 다음 해당 종속성의 종속성을
재귀적으로 가져옵니다. 모든 종속성이 해결되고 다운로드될 때까지
이 작업을 수행하거나 충돌이 발견되면 중지합니다.

Bundler는 충돌하는 종속성을 발견하면 오류를 발생시킵니다. 예를 들어,
젬 A가 젬 Z의 버전 3 이상을 요구하지만 젬 B가 버전 2를 요구하는 경우
Bundler는 충돌을 알려줍니다. 이것은 많은
젬이 다른 젬을 참조(다른 젬을 참조)하여 해결해야 할 큰
종속성 그래프를 형성할 수 있으므로 매우 유용합니다.

# 테스트

테스트는 Ruby 문화의 큰 부분입니다. Ruby에는 minitest(Ruby 버전 1.8.x의 경우 TestUnit)라는
자체 Unit 스타일 테스트 프레임워크가 함께 제공됩니다.
다른 목표를 가진 많은 테스트 라이브러리가 있습니다.

* [TestUnit](http://ruby-doc.org/stdlib-1.8.7/libdoc/test/unit/rdoc/Test/Unit.html) - Ruby 1.8의 내장 "Unit 스타일" 테스트 프레임워크
* [minitest](http://ruby-doc.org/stdlib-2.0.0/libdoc/minitest/rdoc/MiniTest.html) - Ruby 1.9/2.0의 내장 테스트 프레임워크
* [RSpec](http://rspec.info/) - 표현력에 중점을 둔 테스트 프레임워크
* [Cucumber](http://cukes.info/) - Gherkin 형식 테스트를 구문 분석하는 BDD 테스트 프레임워크

## 친절하게 대하기

Ruby 커뮤니티는 개방적이고 다양하며 환영하는 커뮤니티라는 자부심을 가지고 있습니다.
Matz 자신은 매우 친절하며, 전체적으로 루비스트의 관대함은
놀랍습니다.
