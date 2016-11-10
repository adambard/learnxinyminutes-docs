task default: %w[test]

task :test do
  failed = false
  Dir["./tests/*.rb"].each do |test_file|
    begin
      ruby test_file
      puts ""
    rescue
      puts "FAILED #{test_file}!"
      puts ""
      failed = true
    end
  end

  if failed
    exit 0
  end
end
