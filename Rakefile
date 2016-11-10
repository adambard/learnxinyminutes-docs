task default: %w[test]

task :test do
  Dir["./tests/*.rb"].each do |test_file|
    begin
    	ruby test_file
    rescue
      puts "FAILED #{test_file}!"
    end
  end
end
