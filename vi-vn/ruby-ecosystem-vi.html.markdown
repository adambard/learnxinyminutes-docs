---
category: tool
tool: ruby ecosystem
contributors:
    - ["Jon Smock", "http://github.com/jonsmock"]
    - ["Rafal Chmiel", "http://github.com/rafalchmiel"]
    - ["Vinh Nguyen", "http://rubydaily.net"]
lang: vi-vn
---

Nhìn chung các lập trình viên Ruby luôn có cách để cài đặt các phiên bản
Ruby khác nhau, quản lý các gói (hoặc gems), và quản lý các thư viện.

## Trình quản lý Ruby

Một vài nền tảng phải có Ruby đã được cài đặt trước hoặc có sẵn như một gói.
Số đông lập trình viên Ruby không sử dụng cái này, hoặc nếu có, họ chỉ sử
dụng chúng để bootstrap cài đặt Ruby. Thay vào đó, các lập trình viên Ruby
có xu hướng cài đặt trình quản lý Ruby để cài đặt và chuyển đổi các phiên
bản của Ruby và môi trường Ruby cho dự án của họ.

Dưới đây là các trình quản lý môi trường Ruby nổi tiếng:

* [RVM](https://rvm.io/) - Cài đặt và chuyển đổi các phiên bản Ruby. RVM cũng
  có các khái niệm về tập các gems để quản lý môi trường dự án một
  cách tốt nhất.
* [ruby-build](https://github.com/sstephenson/ruby-build) - Chỉ cài đặt các
  phiên bản Ruby. Sử dụng cái này giúp cho việc cài đặt Ruby tốt hơn.
* [rbenv](https://github.com/sstephenson/rbenv) - Chỉ dùng để chuyển đổi các
  phiên bản Ruby. Được sử dụng đi kèm với ruby-build. Tiện ích này sẽ giúp
  cho việc dùng Ruby tốt hơn.
* [chruby](https://github.com/postmodern/chruby) - Chỉ dùng để chuyển đổi các
  phiên bản Ruby. Tương tự như rbenv. Không quan tâm làm thế nào Ruby được
  cài đặt.

## Các phiên bản Ruby

Ruby được tạo ra bởi Yukihiro "Matz" Matsumoto, người được xem như là một
[BDFL](https://en.wikipedia.org/wiki/Benevolent_Dictator_for_Life), mặc dầu gần
đây luôn thay đổi. Kết quả là, tham chiếu của Ruby được gọi là MRI(Matz'
Reference Implementation), và khi bạn biết về một phiên bản Ruby, nó đang
được tham chiếu để phát hành một phiên bản của MRI.

Có ba phiên bản Ruby chính thức được dùng là:

* 2.0.0 - Được phát hành vào tháng 2 năm 2013. Hầu hết các thư viện lớn, và
nền tảng đều hỗ trợ 2.0.0.
* 1.9.3 - Được phát hành vào tháng 10 năm 2011. Đây là phiên bản hầu hết các
lập trình viên Ruby đang dùng. [Nhưng đã không còn hỗ trợ](
  https://www.ruby-lang.org/en/news/2015/02/23/support-for-ruby-1-9-3-has-ended
  /)
* 1.8.7 - [Ruby 1.8.7 đã không còn được sử dụng](
  http://www.ruby-lang.org/en/news/2013/06/30/we-retire-1-8-7/).

Sự thay đổi giữa phiên bản 1.8.7 đến 1.9.x lớn hơn nhiều so với thay đổi từ
1.9.3 đến 2.0.0. Ví dụ, các phiên bản 1.9 giới thiệu các bảng mã và một
byecote VM. Có các dự án vẫn đang ở 1.8.7, nhưng chúng chiếm một số lượng ít
, phần lớn cộng đồng đã chuyển sang ít nhất là 1.9.2 hoặc 1.9.3

## Các ứng dụng Ruby

Hệ sinh thái Ruby có rất nhiều ứng dụng, với mỗi thế mạnh độc đáo và khả
năng tương thích. Để rõ ràng hơn, sự khác nhau giữa các ứng dụng được viết
bằng các ngôn ngữ khác nhau, nhưng *chúng vẫn là Ruby*.
Mỗi ứng dụng có các hook đặc trưng và những tính năng đặc biệt, nhưng tất cả
đều chạy Ruby rất tốt. Ví dụ, JRuby được viết bằng Java, nhưng bạn không
cần biết Java để sử dụng.

Một số ứng dụng nổi tiếng/tương thích cao:

* [MRI](https://github.com/ruby/ruby) - Được viết bằng C, đây là ứng dụng
  tham chiếu của Ruby. Nó tương thích 100%. Tất cả các phiên bản Ruby có khả
  năng duy trì với MRI(xem [RubySpec](#rubyspec) bên dưới).
* [JRuby](http://jruby.org/) - Được viết bằng Java và Ruby, ứng dụng này khá
  nhanh. Điểm mạnh quan trọng nhất của JRuby là JVM/Java interop, tận dụng
  các công cụ, dự án và ngôn ngữ hiện có của JVM.
* [Rubinius](http://rubini.us/) - Được viết bằng ngôn ngữ chính là Ruby với
  một C++ bytecode VM. Rất nhanh. Bởi vì nó được phát triển bằng chính Ruby.

Một số ứng dụng khá nổi tiếng/tương thích:

* [Maglev](http://maglev.github.io/) - Đứng đầu Gemstone, một Smalltalk VM.
  SmallTalk có một vài tiện ích hấp dẫn, và trong dự án này đã mang nó vào
  môi trường Ruby.
* [RubyMotion](http://www.rubymotion.com/) - Mang Ruby đến việc phát triển iOS.

Một số ứng dụng tốt/tương thích:

* [Topaz](http://topazruby.com/) - Được biết bằng RPython (sử dụng Pypy),
  Topaz vẫn còn rất trẻ và chưa hoàn toàn tương thích. Nó hứa hẹn khả năng
  trở thành một ứng dụng Ruby tương thích cao.
* [IronRuby](http://ironruby.net/) - Được viết bằng C# hướng đến nền tảng .NET
  , IronRuby dường như đã dừng hoạt động kể từ khi Microsoft rút hỗ trợ.

Các ứng dụng Ruby có các phiên bản riêng của mình, nhưng chúng luôn luôn
hướng đến sự một phiên bản đặc biệt của MRI cho sự tương thích. Nhiều ứng
dụng có khả năng đến các chế độ khác nhau (ví dụ, 1.8 hoặc 1.9) để hướng đến
phiên bản MRI.

## RubySpec

Hầu hết các ứng dụng Ruby dựa vào [RubySpec](http://rubyspec.org/). Ruby không
có thông báo chính thức, nhưng cộng đồng đã viết những specs thực thi trong
Ruby để kiểm tra sự tương thích với MRI.

## RubyGems

[RubyGems](http://rubygems.org/) là một cộng đồng quản lý các gói cho Ruby.
RubyGems đi kèm với Ruby, bởi vậy không cần cài đặt riêng lẻ.

Các gói Ruby được gọi là "gems", và chúng được host bởi cộng đồng tại
RubyGems.org. Một gem chứa mã nguồn của nó và một vài mô tả, bao gồm những
thứ như phiên bản, các thư viện độc lập, các tác giả và các loại giấy phép.

## Bundler

[Bundler](http://bundler.io/) là một gem giải quyết độc lập. Nó sử dụng một
Gemfile để tìm kiếm các thư viện độc lập trong dự án, và sau đó sẽ lấy về
các thư viện của các thư viện độc lập này. Nó thực hiện cho đến khi việc
tải các thư viện hoàn tất, hoặc nó sẽ dừng nếu xuất hiện bất kỳ xung đột nào.

Bundler sẽ hiển thị lỗi nếu tìm thấy bất kỳ xung đột giữa các thư viện. Ví
dụ, nếu như gem A yêu cầu gem Z có phiên bản 3 hoặc cao hơn, nhưng gem B lại
yêu cầu gem Z phiên bản 2. Bundler sẽ thông báo cho bạn sự xung đột này.
Điều này đã rất hữu ích khi nhiều gem tham chiếu các các gem khác (trong
gem này lại tham chiếu đến các gem khác nữa), có thể hình thành một đồ thị
lớn để nói.

# Kiểm thử

Kiểm thử là một phần lớn của Ruby. Ruby mang đến một nền tảng kiểm thử theo
kiểu Unit được gọi là minitest (hoặc TestUnit for phiên bản Ruby 1.8.x).
Có nhiều thư viện kiểm thử với các mục đích khác nhau.

* [TestUnit](http://ruby-doc.org/stdlib-1.8.7/libdoc/test/unit/rdoc/Test/
  Unit.html) - Nền tảng kiểm thử theo kiểu Unit của Ruby 1.8.
* [minitest](http://ruby-doc.org/stdlib-2.0.0/libdoc/minitest
  /rdoc/MiniTest.html) -Nền tảng kiểm thử được xây dựng cho Ruby 1.9/2.0
* [RSpec](http://rspec.info/) - Một nền tảng kiểm thử tập trung vào sự
  hoạt động.
* [Cucumber](http://cukes.info/) - Một nền tảng kiểm thử theo kiểu BDD dưới
  định dạng Gherkin.

## Be Nice

Cộng đồng Ruby tự hào là một cộng đồng mở, đa dạng và chào đón tất cả mọi
người. Bản thân Matz là một người cực kỳ thân thiện, và các lập trình viên
Ruby rất tuyệt vời.
