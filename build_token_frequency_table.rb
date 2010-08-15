require 'rubygems'
require 'active_record'

if not File.exists?('database.yml')
  exit()
end

dbconfig = YAML::load(File.open('database.yml', 'r'))

ActiveRecord::Base.establish_connection(dbconfig)

class NGram < ActiveRecord::Base
  set_table_name :n_grams
  set_primary_key :id
end

raw_text = File.open('compiled_summaries.txt', 'r') {|f| f.read()}
tokens = raw_text.scan(/[a-z]+[a-z\-']*[a-z]*/)

index = {}
baseline_frequencies = {}

tokens.each do |token|
  if index.key?(token)
    index[token] = index[token] + 1
  else
    index[token] = 1
  end
  
  if not baseline_frequencies.key?(token)
    n_gram = NGram.find(:first, :conditions => {:n_gram => token})
    
    if n_gram.nil?
      baseline_frequencies[token] = ''
    else
      baseline_frequencies[token] = n_gram.probability
    end
  end
end

total_occurrences = tokens.size

frequency_file = File.new('frequency_table.csv', 'w')
frequency_file.puts "Token\tOccurrences\tFrequency\tBaselineFrequency"

index.keys.each do |token|
  frequency_file.puts "#{token}\t#{index[token]}\t#{index[token].to_f / total_occurrences}\t#{baseline_frequencies[token]}"
end

frequency_file.close
