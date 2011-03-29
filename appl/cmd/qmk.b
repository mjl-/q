implement Qmk;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "string.m";
	str: String;
include "names.m";
	names: Names;

Qmk: module {
	init:	fn(nil: ref Draw->Context, nil: list of string);
};

xflag := 0;
root: string;

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	str = load String String->PATH;
	names = load Names Names->PATH;

	arg->init(args);
	arg->setusage(arg->progname()+" [-x] [-r root] args");
	while((c := arg->opt()) != 0)
		case c {
		'x' =>	xflag++;
		'r' =>	root = arg->earg();
		* =>	arg->usage();
		}
	args = arg->argv();
	if(len args == 0)
		arg->usage();

	hdr := str->quoted(args)+"\n";
	datal := list of {array[] of {byte 0}};
	ndatal := 0;
	b := bufio->fopen(sys->fildes(0), Bufio->OREAD);
	if(b == nil)
		fail(sprint("fopen: %r"));
	for(;;) {
		s := b.gets('\n');
		if(s == nil)
			break;
		if(s[len s-1] == '\n')
			s = s[:len s-1];
		(nil, m) := sys->tokenize(s, "\t");
		if(len m != 1 && len m != 2)
			fail(sprint("bad line"));
		name := hd m;
		havepath := len m == 2;
		if(havepath)
			path := hd tl m;
		else
			path = name;
		if(root != nil)
			path = root+"/"+path;
		fd := sys->open(path, Sys->OREAD);
		if(fd == nil)
			fail(sprint("open: %r"));
		(ok, d) := sys->fstat(fd);
		if(ok < 0)
			fail(sprint("stat: %r"));
		isdir := d.mode & Sys->DMDIR;
		length := 0;
		buf: array of byte;
		if(!isdir) {
			length = int d.length;
			if(sys->readn(fd, buf = array[length] of byte, len buf) != len buf)
				fail(sprint("read: %r"));
		}
		mode := "f";
		if(isdir)
			mode = "d";
		hdr += sprint("%s\t%s\t%s\t%d\t%d\n", names->dirname(name), names->basename(name, nil), mode, length, ndatal);
		if(len buf != 0) {
			datal = buf::datal;
			ndatal += len buf;
		}
	}
	out := bufio->fopen(sys->fildes(1), Bufio->OWRITE);
	if(out == nil)
		fail(sprint("fopen: %r"));

	if(xflag)
		xputs(out, "data = array[] of {\n");
	xwrite(out, array of byte hdr);
	for(l := rev(datal); l != nil; l = tl l)
		xwrite(out, hd l);
	if(xflag)
		xputs(out, "};\n");
	if(out.flush() == Bufio->ERROR)
		fail(sprint("flush: %r"));
}

xwrite(b: ref Iobuf, d: array of byte)
{
	if(!xflag) {
		if(b.write(d, len d) != len d)
			fail(sprint("write: %r"));
		return;
	}

	for(i := 0; i < len d; i++) {
		xputs(b, sprint("byte 16r%02x,", int d[i]));
		if(i % 10 == 9)
			xputs(b, "\n");
	}
}

xputs(b: ref Iobuf, s: string)
{
	if(b.puts(s) == Bufio->ERROR)
		fail(sprint("puts: %r"));
}

rev[T](l: list of T): list of T
{
	r: list of T;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

warn(s: string)
{
	sys->fprint(sys->fildes(2), "%s\n", s);
}

fail(s: string)
{
	warn(s);
	raise "fail:"+s;
}
