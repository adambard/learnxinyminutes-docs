---
name: Sorbet
filename: learnsorbet.rb
contributors:
  - ["Jeremy Kaplan", "https://jdkaplan.dev"]
---

Sorbet은 Ruby용 타입 체커입니다. 정적 및 런타임 타입 검사를 모두 활성화하는 메서드 서명 구문을 추가합니다.

가장 쉽게 작동하는 것을 볼 수 있는 방법은 [sorbet.run](https://sorbet.run)의 플레이그라운드입니다.

아래 섹션 중 하나를 복사해 보세요! 각 최상위 `class` 또는 `module`은 다른 것과 독립적입니다.

```ruby
# 모든 파일에는 정적 타입 검사 중에 Sorbet이 얼마나 엄격해야 하는지를 알려주는
# "타입 시길"이 있어야 합니다.
#
# 엄격도 수준 (느슨함에서 엄격함 순):
#
# ignore: Sorbet이 파일을 읽지 않습니다. 즉, 타입 검사 중에
# 해당 내용이 보이지 않습니다. 이것은 피하십시오.
#
# false: Sorbet은 상수 확인과 관련된 오류만 보고합니다.
# 시길이 포함되지 않은 경우 기본값입니다.
#
# true: Sorbet은 모든 정적 타입 오류를 보고합니다. 이것은
# 노력 대비 안전성의 최적점입니다.
#
# strict: Sorbet은 모든 메서드, 상수 및 인스턴스
# 변수에 정적 타입이 있어야 합니다.
#
# strong: Sorbet은 더 이상 명시적으로도 T.untyped를 허용하지 않습니다.
# 거의 아무것도 이것을 만족시키지 못합니다.

# typed: true

# 런타임 타입 검사 라이브러리를 포함합니다. 이렇게 하면 인라인 시그를 작성하고
# 런타임에 검사할 수 있습니다(RBI 전용으로 Sorbet을 실행하는 대신).
# 이러한 런타임 검사는 `ignore` 또는 `false` 시길이 있는 파일에서도 발생합니다.
require 'sorbet-runtime'

class BasicSigs
  # 타입 정의 도우미를 가져옵니다. 거의 항상 필요합니다.
  extend T::Sig

  # 시그는 `sig`와 블록으로 정의됩니다. `returns`로 반환 값 타입을
  # 정의합니다.
  #
  # 이 메서드는 클래스가 `String`인 값을 반환합니다. 이것들은 가장
  # 일반적인 타입이며 Sorbet은 이를 "클래스 타입"이라고 부릅니다.
  sig { returns(String) }
  def greet
    'Hello, World!'
  end

  # `params`로 매개변수 값 타입을 정의합니다.
  sig { params(n: Integer).returns(String) }
  def greet_repeat(n)
    (1..n).map { greet }.join("\n")
  end

  # 키워드 매개변수도 같은 방식으로 정의합니다.
  sig { params(n: Integer, sep: String).returns(String) }
  def greet_repeat_2(n, sep: "\n")
    (1..n).map { greet }.join(sep)
  end

  # 위치/키워드 및 필수/선택 사항은 여기서 차이가 없습니다.
  # 모두 `params`에서 같은 방식으로 정의됩니다.

  # 매개변수가 많은 경우 중괄호 대신 do..end와 여러 줄 블록을
  # 사용하는 것이 더 좋습니다.
  sig do
    params(
      str: String,
      num: Integer,
      sym: Symbol,
    ).returns(String)
  end
  def uhh(str:, num:, sym:)
    'What would you even do with these?'
  end

  # 반환 값이 쓸모없는 메서드의 경우 `void`를 사용합니다.
  sig { params(name: String).void }
  def say_hello(name)
    puts "Hello, #{name}!"
  end

  # 스플랫! "나머지 매개변수", "*args", "**kwargs" 등으로도 알려져 있습니다.
  #
  # `args` 또는 `kwargs` 자체의 값이 아닌 `args` 또는 `kwargs`의
  # _멤버_가 가질 값의 타입을 지정합니다.
  sig { params(args: Integer, kwargs: String).void }
  def no_op(*args, **kwargs)
    if kwargs[:op] == 'minus'
      args.each { |i| puts(i - 1) }
    else
      args.each { |i| puts(i + 1) }
    end
  end

  # 대부분의 초기화자는 `void`여야 합니다.
  sig { params(name: String).void }
  def initialize(name:)
    # 인스턴스 변수는 정적 타입 검사에 참여하려면
    # 주석이 달린 타입이 있어야 합니다.

    # `T.let`의 값은 정적으로 그리고 런타임에 확인됩니다.
    @upname = T.let(name.upcase, String)

    # Sorbet은 이것을 추론할 수 있습니다!
    @name = name
  end

  # 상수에도 주석이 달린 타입이 필요합니다.
  SORBET = T.let('A delicious frozen treat', String)

  # 클래스 변수도 마찬가지입니다.
  @@the_answer = T.let(42, Integer)

  # Sorbet은 `attr_*` 계열을 알고 있습니다.
  sig { returns(String) }
  attr_reader :upname

  sig { params(write_only: Integer).returns(Integer) }
  attr_writer :write_only

  # 리더 부분을 말하면 Sorbet이 라이터 부분을 말할 것입니다.
  sig { returns(String) }
  attr_accessor :name
end

module Debugging
  extend T::Sig

  # 때로는 Sorbet이 표현식에 대해 어떤 타입을 추론했는지 아는 것이
  # 도움이 될 때가 있습니다. `T.reveal_type`을 사용하여 타입 검사에서
  # 해당 정보와 함께 특별한 오류를 표시하도록 합니다.
  #
  # 이것은 Sorbet을 편집기에 통합하여 파일을 저장하자마자
  # 결과를 볼 수 있는 경우에 가장 유용합니다.

  sig { params(obj: Object).returns(String) }
  def debug(obj)
    T.reveal_type(obj) # 공개된 타입: Object
    repr = obj.inspect

    # Ruby 메서드는 인수 없이 호출할 수 있으므로 몇 글자를
    # 절약할 수 있습니다!
    T.reveal_type repr # 공개된 타입: String

    "DEBUG: " + repr
  end
end

module StandardLibrary
  extend T::Sig
  # Sorbet은 Ruby 표준 라이브러리를 타이핑하는 데 도움이 되는 몇 가지
  # 도우미를 제공합니다.

  # `true`와 `false`를 모두 잡으려면 T::Boolean을 사용하십시오.
  #
  # 궁금한 분들을 위해, 이것은 다음과 같습니다.
  #
  #     T.type_alias { T.any(TrueClass, FalseClass) }
  #
  sig { params(str: String).returns(T::Boolean) }
  def confirmed?(str)
    str == 'yes'
  end

  # 값 `nil`은 NilClass의 인스턴스임을 기억하십시오.
  sig { params(val: NilClass).void }
  def only_nil(val:); end

  # 표준 라이브러리 클래스를 수정하지 않기 위해 Sorbet은 일반적인
  # 제네릭을 지원하는 래퍼를 제공합니다.
  #
  # 전체 목록은 다음과 같습니다:
  # * T::Array
  # * T::Enumerable
  # * T::Enumerator
  # * T::Hash
  # * T::Range
  # * T::Set
  sig { params(config: T::Hash[Symbol, String]).returns(T::Array[String]) }
  def merge_values(config)
    keyset = [:old_key, :new_key]
    config.each_pair.flat_map do |key, value|
      keyset.include?(key) ? value : 'sensible default'
    end
  end

  # 때로는 (보통 의존성 주입) 메서드가 클래스의 인스턴스가 아닌
  # 클래스에 대한 참조를 허용합니다. `Dep` 클래스 자체(또는
  # 상속받는 것)를 허용하려면 `T.class_of(Dep)`를 사용하십시오.
  class Dep; end

  sig { params(dep: T.class_of(Dep)).returns(Dep) }
  def dependency_injection(dep:)
    dep.new
  end

  # 블록, proc 및 람다, 오 세상에! 이 모든 것은 `T.proc`으로 타입이 지정됩니다.
  #
  # 제한 사항:
  # 1. 모든 매개변수는 필수 위치 매개변수로 가정됩니다.
  # 2. 유일한 런타임 검사는 값이 `Proc`이라는 것입니다. 인수 타입은
  #    정적으로만 확인됩니다.
  sig do
    params(
      data: T::Array[String],
      blk: T.proc.params(val: String).returns(Integer),
    ).returns(Integer)
  end
  def count(data, &blk)
    data.sum(&blk)
  end

  sig { returns(Integer) }
  def count_usage
    count(["one", "two", "three"]) { |word| word.length + 1 }
  end

  # 메서드에 암시적 블록이 있는 경우 Sorbet은 `T.untyped`를 추론합니다.
  # 타입이 중요한 경우 명시적 블록 구문을 사용하십시오.
  sig { params(str: String).returns(T.untyped) }
  def implicit_block(str)
    yield(str)
  end

  # DSL을 작성하고 다른 컨텍스트에서 블록을 실행하는 경우
  # `bind`를 사용하십시오.
  sig { params(num: Integer, blk: T.proc.bind(Integer).void).void }
  def number_fun(num, &blk)
    num.instance_eval(&blk)
  end

  sig { params(num: Integer).void }
  def number_fun_usage(num)
    number_fun(10) { puts digits.join }
  end

  # 블록에 매개변수가 없는 경우 `params`를 포함하지 마십시오.
  sig { params(blk: T.proc.returns(Integer)).returns(Integer) }
  def doubled_block(&blk)
    2 * blk.call
  end
end

module Combinators
  extend T::Sig
  # 이러한 메서드를 사용하면 기존 타입에서 새 타입을 정의할 수 있습니다.

  # 여러 타입 중 하나일 수 있는 값이 있는 경우 `T.any`를 사용하십시오.
  # 이것들은 때때로 "유니온 타입" 또는 "합 타입"으로 알려져 있습니다.
  sig { params(num: T.any(Integer, Float)).returns(Rational) }
  def hundreds(num)
    num.rationalize
  end

  # `T.nilable(Type)`은 `T.any(Type, NilClass)`의 편리한 별칭입니다.
  sig { params(val: T.nilable(String)).returns(Integer) }
  def strlen(val)
    val.nil? ? -1 : val.length
  end

  # 여러 타입을 만족해야 하는 값이 있는 경우 `T.all`을 사용하십시오.
  # 이것들은 때때로 "교차 타입"으로 알려져 있습니다. 인터페이스(나중에
  # 설명)에 가장 유용하지만 도우미 모듈을 설명하는 데도 사용할 수 있습니다.

  module Reversible
    extend T::Sig
    sig { void }
    def reverse
      # 실제로 구현된 척
    end
  end

  module Sortable
    extend T::Sig
    sig { void }
    def sort
      # 실제로 구현된 척
    end
  end

  class List
    include Reversible
    include Sortable
  end

  sig { params(list: T.all(Reversible, Sortable)).void }
  def rev_sort(list)
    # Reversible의 reverse
    list.reverse
    # Sortable의 sort
    list.sort
  end

  def rev_sort_usage
    rev_sort(List.new)
  end

  # 때로는 실제로 모든 시간을 타입을 명시하는 것이 도움이 되기보다
  # 더 혼란스러울 때가 있습니다. 타입 별칭을 사용하여 더 쉽게
  # 작업할 수 있습니다.
  JSONLiteral = T.type_alias { T.any(Float, String, T::Boolean, NilClass) }

  sig { params(val: JSONLiteral).returns(String) }
  def stringify(val)
    val.to_s
  end
end

module DataClasses
  extend T::Sig
  # `T::Struct`를 사용하여 타입 검사 필드가 있는 새 클래스를 만듭니다.
  # 표준 Struct와 OpenStruct의 가장 좋은 부분을 결합한 다음
  # 정적 타이핑을 추가합니다.
  #
  # 이 방법으로 구성된 타입은 때때로 "곱 타입"으로 알려져 있습니다.

  class Matcher < T::Struct
    # `prop`을 사용하여 리더와 라이터가 모두 있는 필드를 정의합니다.
    prop :count, Integer
    # `const`를 사용하여 리더만 정의하고 라이터는 건너뜁니다.
    const :pattern, Regexp
    # `default`로 기본값을 설정할 수 있습니다.
    const :message, String, default: 'Found one!'

    # 이것은 그렇지 않으면 일반 클래스이므로 여전히 메서드를 정의할 수 있습니다.

    # 사용하려면 여전히 `sig`를 가져와야 합니다.
    extend T::Sig

    sig { void }
    def reset
      self.count = 0
    end
  end

  sig { params(text: String, matchers: T::Array[Matcher]).void }
  def awk(text, matchers)
    matchers.each(&:reset)
    text.lines.each do |line|
      matchers.each do |matcher|
        if matcher.pattern =~ line
          Kernel.puts matcher.message
          matcher.count += 1
        end
      end
    end
  end

  # 함정과 제한 사항

  # 1. `const` 필드는 진정으로 불변이 아닙니다. 라이터 메서드는 없지만
  #    다른 방식으로 변경될 수 있습니다.
  class ChangeMe < T::Struct
    const :list, T::Array[Integer]
  end

  sig { params(change_me: ChangeMe).returns(T::Boolean) }
  def whoops!(change_me)
    change_me = ChangeMe.new(list: [1, 2, 3, 4])
    change_me.list.reverse!
    change_me.list == [4, 3, 2, 1]
  end

  # 2. `T::Struct`는 `BasicObject`에서 동등성 메서드를 상속하며,
  #    이는 항등 동등성(참조 동등성이라고도 함)을 사용합니다.
  class Coordinate < T::Struct
    const :row, Integer
    const :col, Integer
  end

  sig { returns(T::Boolean) }
  def never_equal!
    p1 = Coordinate.new(row: 1, col: 2)
    p2 = Coordinate.new(row: 1, col: 2)
    p1 != p2
  end

  # 원하는 경우 필드를 확인하는 자신만의 `#==` 메서드를 정의하십시오.
  class Position < T::Struct
    extend T::Sig

    const :x, Integer
    const :y, Integer

    sig { params(other: Object).returns(T::Boolean) }
    def ==(other)
      # 실제 구현은 여기에 있습니다:
      # https://github.com/tricycle/sorbet-struct-comparable
      true
    end
  end

  # `T::Enum`을 사용하여 쉽게 참조할 수 있는 고정된 값 집합을 정의합니다.
  # 이것은 값 자체가 무엇인지보다 가능성 집합이 닫혀 있고
  # 정적이라는 점에 더 신경 쓰는 경우에 특히 유용합니다.
  class Crayon < T::Enum
    extend T::Sig

    # `enums`로 멤버를 초기화합니다.
    enums do
      # 각 멤버를 `new`로 정의합니다. 이들 각각은
      # `Crayon` 클래스의 인스턴스입니다.
      Red = new
      Orange = new
      Yellow = new
      Green = new
      Blue = new
      Violet = new
      Brown = new
      Black = new
      # 열거형의 기본값은 모두 소문자로 된 이름입니다.
      # 변경하려면 `new`에 값을 전달하십시오.
      Gray90 = new('light-gray')
    end

    sig { returns(String) }
    def to_hex
      case self
      when Red   then '#ff0000'
      when Green then '#00ff00'
      # ...
      else            '#ffffff'
      end
    end
  end

  sig { params(crayon: Crayon, path: T::Array[Position]).void }
  def draw(crayon:, path:)
    path.each do |pos|
      Kernel.puts "(#{pos.x}, #{pos.y}) = " + crayon.to_hex
    end
  end

  # 열거형의 모든 값을 얻으려면 `.values`를 사용하십시오. 편의를 위해
  # 열거형 문자열 값을 얻는 `#serialize`가 이미 있습니다.

  sig { returns(T::Array[String]) }
  def crayon_names
    Crayon.values.map(&:serialize)
  end

  # 문자열에서 열거형 값으로 이동하려면 "deserialize" 계열을 사용하십시오.

  sig { params(name: String).returns(T.nilable(Crayon)) }
  def crayon_from_name(name)
    if Crayon.has_serialized?(name)
      # 값이 없는 경우 `KeyError`가 발생합니다.
      Crayon.deserialize(name)
    end

    # 값이 없는 경우 `nil`을 반환합니다.
    Crayon.try_deserialize(name)
  end
end

module FlowSensitivity
  extend T::Sig
  # Sorbet은 Ruby의 제어 흐름 구문을 이해하고 해당 정보를 사용하여
  # 코드가 분기될 때 더 정확한 타입을 얻습니다.

  # nil 검사를 할 때 가장 자주 보게 될 것입니다.
  sig { params(name: T.nilable(String)).returns(String) }
  def greet_loudly(name)
    if name.nil?
      'HELLO, YOU!'
    else
      # Sorbet은 `name`이 여기서 String이어야 함을 알고 있으므로 `#upcase`를
      # 호출하는 것이 안전합니다.
      "HELLO, #{name.upcase}!"
    end
  end

  # nil은 `T.any`를 구체화하는 특별한 경우입니다.
  sig { params(id: T.any(Integer, T::Array[Integer])).returns(T::Array[String]) }
  def database_lookup(id)
    if id.is_a?(Integer)
      # 여기서 `ids`는 Integer여야 합니다.
      [id.to_s]
    else
      # 여기서 `ids`는 T::Array[Integer]여야 합니다.
      id.map(&:to_s)
    end
  end

  # Sorbet은 타입 정의를 좁히는 다음 메서드를 인식합니다:
  # * is_a?
  # * kind_of?
  # * nil?
  # * Class#===
  # * Class#<
  # * block_given?
  #
  # 매우 일반적이므로 다음 Rails 확장도 인식합니다:
  # * blank?
  # * present?
  #
  # 이러한 메서드를 재정의하는 경우 Sorbet 가정을 유지하도록 주의하십시오!

  # 이 코드 줄을 작성한 적이 있습니까?
  #
  #     raise StandardError, "Can't happen"
  #
  # Sorbet은 `T.absurd`를 사용하여 정적으로 이를 증명하는 데 도움이 될 수 있습니다
  # (이것은 "철저함"으로 알려져 있습니다). `T::Enum`과 결합하면
  # 더욱 멋집니다!

  class Size < T::Enum
    extend T::Sig

    enums do
      Byte = new('B')
      Kibibyte = new('KiB')
      Mebibyte = new('MiB')
      # "640K ought to be enough for anybody"
    end

    sig { returns(Integer) }
    def bytes
      case self
        when Byte     then 1 <<  0
        when Kibibyte then 1 << 10
        when Mebibyte then 1 << 20
        else
          # Sorbet은 모든 경우를 확인했음을 알고 있으므로 `self`가
          # 여기서 가질 수 있는 가능한 값은 없습니다.
          #
          # 하지만 어떻게든 여기에 도달하면 런타임에 오류가 발생합니다.
          T.absurd(self)

          # 누락된 케이스가 있는 경우 Sorbet은 어떤 것인지 알려줄 수도 있습니다!
      end
    end
  end

  # 다음 부분에는 `puts`와 `raise`가 필요합니다.
  include Kernel

  # Sorbet은 `raise` 문 뒤에는 코드를 실행할 수 없다는 것을 알고 있습니다.
  # 왜냐하면 "절대 반환하지 않기" 때문입니다.
  sig { params(num: T.nilable(Integer)).returns(Integer) }
  def decrement(num)
    raise ArgumentError, '¯\_(ツ)_/¯' unless num

    num - 1
  end

  class CustomError < StandardError; end

  # `T.noreturn`으로 자신만의 오류 발생 메서드에 주석을 달 수 있습니다.
  sig { params(message: String).returns(T.noreturn) }
  def oh_no(message = 'A bad thing happened')
    puts message
    raise CustomError, message
  end

  # 무한 루프도 반환하지 않습니다.
  sig { returns(T.noreturn) }
  def loading
    loop do
      %q(-\|/).each_char do |c|
        print "\r#{c} reticulating splines..."
        sleep 1
      end
    end
  end

  # Sorbet이 타입 구체화를 "잃어버리는" 상황에 직면할 수 있습니다.
  # Ruby에서 하는 거의 모든 것이 다음 번에 호출할 때 다른 값을
  # 반환할 수 있는 메서드 호출이라는 것을 기억하십시오. Sorbet은
  # (`attr_reader` 및 `attr_accessor`의 메서드조차도) 어떤 메서드도
  # 순수하다고 가정하지 않습니다.
  sig { returns(T.nilable(Integer)) }
  def answer
    rand > 0.5 ? 42 : nil
  end

  sig { void }
  def bad_typecheck
    if answer.nil?
      0
    else
      # 하지만 다시 호출하면 answer가 `nil`을 반환할 수 있습니다!
      answer + 1
      # ^ 메서드 +가 T.nilable(Integer)의 NilClass 구성 요소에 존재하지 않음
    end
  end

  sig { void }
  def good_typecheck
    ans = answer
    if ans.nil?
      0
    else
      # 이번에는 Sorbet이 `ans`가 nil이 아님을 알고 있습니다.
      ans + 1
    end
  end
end

module InheritancePatterns
  extend T::Sig

  # 항상 수신자의 타입을 반환하는 메서드가 있는 경우
  # `T.self_type`을 사용하십시오. 이것은 유창한 인터페이스와 DSL에서
  # 일반적입니다.
  #
  # 경고: 이 기능은 아직 실험적입니다!
  class Logging
    extend T::Sig

    sig { returns(T.self_type) }
    def log
      pp self
      self
    end
  end

  class Data < Logging
    extend T::Sig

    sig { params(x: Integer, y: String).void }
    def initialize(x: 0, y: '')
      @x = x
      @y = y
    end

    # 관련 클래스가 하나뿐인 경우 `T.self_type`을 사용할 필요는 없습니다.
    sig { params(x: Integer).returns(Data) }
    def setX(x)
      @x = x
      self
    end

    sig { params(y: String).returns(Data) }
    def setY(y)
      @y = y
      self
    end
  end

  # 짜잔!
  sig { params(data: Data).void }
  def chaining(data)
    data.setX(1).log.setY('a')
  end

  # 클래스 메서드(싱글톤 메서드라고도 함)인 경우 `T.attached_class`를 사용하십시오.
  #
  # 여기에는 경고가 없습니다. 이것은 안정적입니다!
  class Box
    extend T::Sig

    sig { params(contents: String, weight: Integer).void }
    def initialize(contents, weight)
      @contents = contents
      @weight = weight
    end

    sig { params(contents: String).returns(T.attached_class) }
    def self.pack(contents)
      new(contents, contents.chars.uniq.length)
    end
  end

  class CompanionCube < Box
    extend T::Sig

    sig { returns(String) }
    def pick_up
      "♥#{@contents}🤍"
    end
  end

  sig { returns(String) }
  def befriend
    CompanionCube.pack('').pick_up
  end

  # Sorbet은 추상 클래스와 인터페이스를 지원합니다. 모든
  # 구체적인 클래스와 구현이 실제로 호환되는
  # 서명으로 필요한 메서드를 정의하는지 확인할 수 있습니다.

  # 추상 클래스는 다음과 같습니다:

  class WorkflowStep
    extend T::Sig

    # 상속 도우미를 가져옵니다.
    extend T::Helpers

    # 이 클래스를 추상으로 표시합니다. 즉, `.new`로 인스턴스화할 수 없지만
    # 여전히 서브클래스화할 수 있습니다.
    abstract!

    sig { params(args: T::Array[String]).void }
    def run(args)
      pre_hook
      execute(args)
      post_hook
    end

    # 이것은 추상 메서드이므로 서브클래스에서 _반드시_
    # 구현해야 합니다. Sorbet에 알리려면 빈 메서드에
    # `abstract`가 있는 서명을 추가하십시오.
    #
    # 이 메서드의 구현이 실제로 런타임에 호출되면
    # `NotImplementedError`가 발생합니다.
    sig { abstract.params(args: T::Array[String]).void }
    def execute(args); end

    # 다음 비추상 메서드는 서브클래스에서 구현할 수 있지만
    # 선택 사항입니다.

    sig { void }
    def pre_hook; end

    sig { void }
    def post_hook; end
  end

  class Configure < WorkflowStep
    extend T::Sig

    sig { void }
    def pre_hook
      puts 'Configuring...'
    end

    # 추상 메서드를 구현하려면 서명에 `override`를 표시하십시오.
    sig { override.params(args: T::Array[String]).void }
    def execute(args)
      # ...
    end
  end

  # 그리고 인터페이스는 다음과 같습니다:

  module Queue
    extend T::Sig

    # 상속 도우미를 가져옵니다.
    extend T::Helpers

    # 이 모듈을 인터페이스로 표시합니다. 그러면 다음 제한 사항이 추가됩니다:
    # 1. 모든 메서드는 추상이어야 합니다.
    # 2. private 또는 protected 메서드를 가질 수 없습니다.
    interface!

    sig { abstract.params(num: Integer).void }
    def push(num); end

    sig { abstract.returns(T.nilable(Integer)) }
    def pop; end
  end

  class PriorityQueue
    extend T::Sig

    # 이 클래스가 인터페이스를 구현한다는 것을 Sorbet에 알리기 위해
    # 인터페이스를 포함합니다. Sorbet은 암시적으로 구현된 인터페이스
    # ("덕 타이핑"이라고도 함)를 지원하지 않습니다.
    include Queue

    sig { void }
    def initialize
      @items = T.let([], T::Array[Integer])
    end

    # Queue 인터페이스의 추상 메서드를 구현합니다. `override`를
    # 사용하는 것을 잊지 마십시오!

    sig { override.params(num: Integer).void }
    def push(num)
      @items << num
      @items.sort!
    end

    sig { override.returns(T.nilable(Integer)) }
    def pop
      @items.shift
    end
  end

  # 모듈에서 클래스 메서드를 얻기 위해 `included` 후크를 사용하는 경우
  # 타입 검사를 위해 `mixes_in_class_methods`를 사용해야 합니다.

  module Mixin
    extend T::Helpers
    interface!

    module ClassMethods
      extend T::Sig

      sig { void }
      def whisk
        'fskfskfsk'
      end
    end

    mixes_in_class_methods(ClassMethods)
  end

  class EggBeater
    include Mixin
  end

  EggBeater.whisk # Meringue!
end

module EscapeHatches
  extend T::Sig

  # Ruby는 매우 동적인 언어이며 때로는 Sorbet이 이미
  # 사실이라고 알고 있는 속성을 추론할 수 없습니다. Sorbet이
  # 안전성을 증명할 수 있도록 코드를 다시 작성하는 방법이 있지만,
  # 이러한 "탈출구"를 사용하여 Sorbet에서 "벗어날" 수도 있습니다.

  # `T.nilable`을 사용하기 시작하면 Sorbet은 nil을 처리하지 않는
  # _모든_ 곳을 알려주기 시작합니다. 때로는 값이 nil이 될 수 없다는 것을
  # 알고 있지만 Sorbet이 증명할 수 있도록 시그를 수정하는 것이
  # 실용적이지 않은 경우가 있습니다. 이 경우 `T.must`를 사용할 수 있습니다.
  sig { params(maybe_str: T.nilable(String)).returns(String) }
  def no_nils_here(maybe_str)
    # maybe_str이 실제로 nil이면 런타임에 오류가 발생합니다.
    str = T.must(maybe_str)
    str.downcase
  end

  # 더 일반적으로, 값이 특정 타입이어야 한다는 것을 알고 있는 경우
  # `T.cast`를 사용할 수 있습니다.
  sig do
    params(
      str_or_ary: T.any(String, T::Array[String]),
      idx_or_range: T.any(Integer, T::Range[Integer]),
    ).returns(T::Array[String])
  end
  def slice2(str_or_ary, idx_or_range)
    # 어떤 이유로든 문자열에서 개별 문자 또는
    # 배열에서 하위 배열을 원한다고 가정해 봅시다. 다른 옵션은 허용되지 않습니다.
    if str_or_ary.is_a?(String)
      # 여기서 `idx_or_range`는 단일 인덱스여야 함을 알고 있습니다. 그렇지 않으면
      # 런타임에 오류가 발생합니다.
      idx = T.cast(idx_or_range, Integer)
      [str_or_ary.chars.fetch(idx)]
    else
      # 여기서 `idx_or_range`는 범위여야 함을 알고 있습니다. 그렇지 않으면
      # 런타임에 오류가 발생합니다.
      range = T.cast(idx_or_range, T::Range[Integer])
      str_or_ary.slice(range) || []
    end
  end

  # 메서드가 존재하지만 Sorbet이 모르는 경우 `T.unsafe`를
  # 사용하여 Sorbet이 호출하도록 할 수 있습니다. 이것을 "안전하지 않은
  # 메서드 호출"로 생각하는 경향이 있지만, `T.unsafe`는 전체
  # 표현식이 아닌 수신자에서 호출됩니다.
  sig { params(count: Integer).returns(Date) }
  def the_future(count)
    # Sorbet이 찾을 수 없는 추가 날짜 도우미를 정의했다고 가정해 봅시다.
    # 따라서 `2.decades`는 ActiveSupport의 `(2*10).years`와 효과적으로 동일합니다.
    Date.today + T.unsafe(count).decades
  end

  # 이것이 암시적 `self`의 메서드인 경우 `T.unsafe`를 사용하려면
  # 명시적으로 만들어야 합니다.
  sig { params(count: Integer).returns(Date) }
  def the_past(count)
    # 메타프로그래밍이 `Time.new`에 대한 `now` 도우미 메서드를
    # 정의한다고 가정해 봅시다. 일반적으로 다음과 같이 보입니다:
    #
    #     now - 1234
    #
    T.unsafe(self).now - 1234
  end

  # Sorbet에는 `T.untyped`라는 특별한 타입이 있습니다. 이 타입의
  # 모든 값에 대해 Sorbet은 모든 메서드 인수에 사용되고
  # 모든 메서드 호출을 수신하도록 허용합니다.

  sig { params(num: Integer, anything: T.untyped).returns(T.untyped) }
  def nothing_to_see_here(num, anything)
    anything.digits # 정수인가...
    anything.upcase # ...아니면 문자열인가?

    # Sorbet은 타입이 지정되지 않았기 때문에 이 반환 값에 대해
    # 아무것도 추론할 수 없습니다.
    BasicObject.new
  end

  def see_here
    # 실제로는 nil입니다! 런타임에 충돌하지만 Sorbet은 허용합니다.
    nothing_to_see_here(1, nil)
  end

  # 시그가 없는 메서드의 경우 Sorbet은 각 인수와
  # 반환 값의 타입을 `T.untyped`로 추론합니다.
end

# 다음 타입은 공식적으로 문서화되지 않았지만 여전히 유용합니다.
# 실험적이거나, 더 이상 사용되지 않거나, 지원되지 않을 수 있습니다.

module ValueSet
  extend T::Sig

  # Ruby의 일반적인 패턴은 옵션 집합에서 하나의 값을 허용하는
  # 메서드를 갖는 것입니다. 특히 Sorbet을 처음 시작할 때
  # 코드를 리팩토링하여 `T::Enum`을 사용하는 것이 실용적이지 않을 수 있습니다.
  # 이 경우 `T.enum`을 사용할 수 있습니다.
  #
  # 참고: Sorbet은 값 자체를 추적하지 않기 때문에 정적으로
  # 확인할 수 없습니다.
  sig do
    params(
      data: T::Array[Numeric],
      shape: T.enum([:circle, :square, :triangle])
    ).void
  end
  def plot_points(data, shape: :circle)
    data.each_with_index do |y, x|
      Kernel.puts "#{x}: #{y}"
    end
  end
end

module Generics
  extend T::Sig

  # 제네릭은 메서드 타입이 포함된 데이터에 따라 변경되는
  # 클래스나 메서드 타입이 인수에 따라 변경되는 메서드가 있는
  # 경우에 유용합니다.

  # 제네릭 메서드는 `type_parameters`를 사용하여 타입 변수를 선언하고
  # `T.type_parameter`를 사용하여 다시 참조합니다.
  sig do
    type_parameters(:element)
      .params(
        element: T.type_parameter(:element),
        count: Integer,
      ).returns(T::Array[T.type_parameter(:element)])
  end
  def repeat_value(element, count)
    count.times.each_with_object([]) do |elt, ary|
      ary << elt
    end
  end

  sig do
    type_parameters(:element)
      .params(
        count: Integer,
        block: T.proc.returns(T.type_parameter(:element)),
      ).returns(T::Array[T.type_parameter(:element)])
  end
  def repeat_cached(count, &block)
    elt = block.call
    ary = []
    count.times do
      ary << elt
    end
    ary
  end

  # 제네릭 클래스는 `T::Generic.type_member`를 사용하여 일반
  # 타입 이름과 같은 타입 변수를 정의합니다.
  class BidirectionalHash
    extend T::Sig
    extend T::Generic

    Left = type_member
    Right = type_member

    sig { void }
    def initialize
      @left_hash = T.let({}, T::Hash[Left, Right])
      @right_hash = T.let({}, T::Hash[Right, Left])
    end

    # 아래 메서드가 작동하도록 충분히 구현합니다.

    sig { params(lkey: Left).returns(T::Boolean) }
    def lhas?(lkey)
      @left_hash.has_key?(lkey)
    end

    sig { params(rkey: Right).returns(T.nilable(Left)) }
    def rget(rkey)
      @right_hash[rkey]
    end
  end

  # 제네릭 타입을 특수화하려면 대괄호를 사용하십시오.
  sig do
    params(
      options: BidirectionalHash[Symbol, Integer],
      choice: T.any(Symbol, Integer),
    ).returns(T.nilable(String))
  end
  def lookup(options, choice)
    case choice
    when Symbol
      options.lhas?(choice) ? choice.to_s : nil
    when Integer
      options.rget(choice).to_s
    else
      T.absurd(choice)
    end
  end

  # 상속을 통해 특수화하려면 `fixed`로 `type_member`를 다시
  # 선언하십시오.
  class Options < BidirectionalHash
    Left = type_member(fixed: Symbol)
    Right = type_member(fixed: Integer)
  end

  sig do
    params(
      options: Options,
      choice: T.any(Symbol, Integer),
    ).returns(T.nilable(String))
  end
  def lookup2(options, choice)
    lookup(options, choice)
  end

  # `type_member`에 추가할 수 있는 다른 분산 주석이 있지만
  # 거의 사용되지 않습니다.
end
```

## 추가 자료

- [공식 문서](https://sorbet.org/docs/overview)
- [sorbet.run](https://sorbet.run) - 플레이그라운드
