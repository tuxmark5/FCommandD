#! /usr/bin/ruby

lines = STDIN.readlines 
ids   = [];

lines.each do |l|
  r = /^#define (\w+)\s+(\w+).*$/.match l
  next if r.nil?
  r1 = r[1];
  r2 = r[2];
  if r1.start_with? "KEY_"
    r2 = "key" + r2;
  elsif r1.start_with? "BTN_"
    r2 = "btn" + r2;
  else
    puts(r1);
    exit(0);
  end;

  ids << [r1, r2]
end

#ids.map! do |i|
#  i1 = i.gsub("BUS_", "bus")
#  
#  [i, i1]
#end;

<<AA

ids.sort.each do |i|
  puts(", %s" % [i[1]]);
end;

ids.sort.each do |i|
  puts("%-20s:: Int32" % [i[1]]);
end;

ids.each do |i|
  puts("%-20s= #const %s" % [i[1], i[0]]);
end;
AA

ids.sort.each do |i|
  name = i[1]
  if name.start_with? "key"
    name = name[3..-1]
  elsif name.start_with? "btn"
    name = "@" + name[3..-1]
  else
    puts "ERR"
  end;
  puts("  , (\"%s\",%s%-20s)" % [name, ' ' * (20 - name.length), i[1]]);
end;
