task default: %w[encoding yaml return_code]
$failure = 0
task :encoding do
  begin
    ruby 'tests/encoding.rb'
  rescue Exception => msg
    puts msg
    $failure += 1
  end
end
task :yaml do
  begin
    ruby 'tests/yaml.rb'
  rescue Exception => msg
    puts msg
    $failure += 1
  end
end
task :return_code do
  if $failure != 0
    raise "Failed #{$failure} tests!!"
  end
end
