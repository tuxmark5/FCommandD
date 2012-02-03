#! /usr/bin/ruby

lines = STDIN.readlines 
ids   = [];

lines.each do |l|
  r = /^#define (\w+)\s+(\w+)$/.match l
  next if r.nil?
  ids << [r[1], r[2]]
end

#ids.map! do |i|
#  i1 = i.gsub("BUS_", "bus")
#  
#  [i, i1]
#end;

ids.each do |i|
  puts(", %s" % [i[1]]);
end;

ids.each do |i|
  puts("%-16s :: Int32" % [i[1]]);
end;

ids.each do |i|
  puts("%-16s = #const %-16s" % [i[1], i[0]]);
end;
