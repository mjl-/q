implement Q;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";

include "data.b";
data: array of byte;

Q: module {
	init:	fn(nil: ref Draw->Context, nil: list of string);
};
Cmd: type Q;

Hdrfile: adt {
	dir,
	name:	string;
	isdir,
	length,
	offset:	int;	# in data
};

dflag := 0;
startargs: list of string;	# module & args to load & call init on
files: array of ref Hdrfile;	# files to mount

init(ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	sys->pctl(Sys->FORKNS, nil);

	if(len args > 2)
		fail("usage: q [file]");
	if(len args == 2)
		err := readdata(hd tl args);
	if(err == nil)
		err = parseheader();
	if(err != nil)
		fail(err);
	mountfiles();
	mod := load Cmd hd startargs;
	if(mod == nil)
		fail(sprint("load: %r"));
	mod->init(ctxt, startargs);
}

readdata(f: string): string
{
	fd := sys->open(f, Sys->OREAD);
	if(fd == nil)
		return sprint("open: %r");
	(ok, dir) := sys->fstat(fd);
	if(ok < 0)
		return sprint("fstat: %r");
	if(sys->readn(fd, data = array[int dir.length] of byte, len data) != len data)
		return sprint("read: %r");
	return nil;
}

# read quoted list of args
unquoted(s: string): (list of string, string)
{
	l: list of string;
	w: string;
	W := q := 0;
More:
	for(i := 0; i < len s; i++)
		case c := s[i] {
		'\'' =>
			if(q) {
				if(i+1 < len s && s[i+1] == '\'') {
					w[len w] = '\'';
					i++;
				} else
					q = 0;
			} else
				W = q = 1;
				
		'\n' =>
			if(!q) {
				i++;
				break More;
			}
			w[len w] = c;
		' ' or
		'\t' =>
			if(q)
				w[len w] = c;
			else if(W || w != nil) {
				l = w::l;
				w = nil;
				W = 0;
			}
		* =>
			w[len w] = c;
		}
	if(W || w != nil)
		l = w::l;
	return (rev(l), s[i:]);
}

parseheader(): string
{
	for(i := 0; i < len data && data[i] != byte 0; i++)
		{}
	if(i >= len data)
		return "missing end of header";
	s := string data[:i];
	i++;
	(startargs, s) = unquoted(s);
	if(startargs == nil)
		return "empty startargs";
	say(sprint("startmod %q, len args %d", hd startargs, len startargs));
	(nil, l) := sys->tokenize(s, "\n");
	files = array[len l] of ref Hdrfile;
	n := 0;
	for(; l != nil; l = tl l) {
		# dir filename type length offset
		(nil, m) := sys->tokenize(hd l, "\t");
		if(len m != 5)
			return sprint("bad line for file %d, %d elems instead of 5", n+1, len m);
		mm := l2a(m);
		isdir: int;
		case mm[2] {
		"d" =>	isdir = 1;
		"f" =>	isdir = 0;
		* =>	return sprint("bad file type %q", mm[2]);
		}
		f := files[n++] = ref Hdrfile(mm[0], mm[1], isdir, int mm[3], i+int mm[4]);
		say(sprint("line %q", hd l));
		say(sprint("Hdrfile(dir %q, name %q, type %s, length %d, offset %d)", f.dir, f.name, mm[2], f.length, f.offset));
		if(f.offset+f.length > len data)
			return sprint("file %q points past end of file", f.name);
	}
	return nil;
}

mountfiles()
{
	for(i := 0; i < len files; i++) {
		if(sys->pipe(fds := array[2] of ref Sys->FD) < 0)
			fail(sprint("pipe: %r"));
		f := files[i];
		spawn srv(f, i*2, i*2+1, fds[1]);
		say(sprint("mount on %q", f.dir));
		if(sys->mount(fds[0], nil, f.dir, Sys->MAFTER, nil) < 0)
			fail(sprint("mount: %r"));
	}
}


Version, Auth, Attach, Error, Flush, Walk, Open, Create, Read, Write, Clunk, Remove, Stat, Wstat: con 100+2*iota;
Notag: con 16rffff;
Nofid: con int 16rffffffff;

Buf: adt {
	d:	array of byte;
	o:	int;

	g8:	fn(b: self ref Buf): int;
	g16:	fn(b: self ref Buf): int;
	g32:	fn(b: self ref Buf): int;
	g64:	fn(b: self ref Buf): big;
	gn:	fn(b: self ref Buf, n: int): array of byte;
	gstr:	fn(b: self ref Buf): string;
};

Buf.g8(b: self ref Buf): int		{ return int b.d[b.o++]; }
Buf.g16(b: self ref Buf): int		{ v := b.g8(); return v|b.g8()<<8; }
Buf.g32(b: self ref Buf): int		{ v := b.g16(); return v|b.g16()<<16; }
Buf.g64(b: self ref Buf): big		{ v := big b.g32(); return v|big b.g32()<<32; }
Buf.gn(b: self ref Buf, n: int): array of byte	{ r := b.d[b.o:b.o+n]; b.o += n; return r; }
Buf.gstr(b: self ref Buf): string	{ n := b.g16(); return string b.gn(n); }

m8(v: int): array of byte	{ return array[] of {byte v}; }
m16(v: int): array of byte	{ return array[] of {byte (v>>0), byte (v>>8)}; }
m32(v: int): array of byte	{ return array[] of {byte (v>>0), byte (v>>8), byte (v>>16), byte (v>>24)}; }
m64(v: big): array of byte	{ d := array[8] of byte; d[:] = m32(int v); d[4:] = m32(int (v>>32)); return d; }
mstr(s: string): array of byte	{ x := array of byte s; d := array[2+len x] of byte; d[:] = m16(len x); d[2:] = x; return d; }
mqid(t: Tabfile): array of byte
{
	d := array[1+4+8] of byte;
	d[:] = m8(t.mode>>24);
	d[1:] = m32(0);
	d[1+4:] = m64(big t.qp);
	return d;
}
mstat(t: Tabfile): array of byte
{
	l := list of {
	# size is prepended by put
	m16(0),
	m32(0),
	mqid(t),
	m32(t.mode),
	m32(0),
	m32(0),
	m64(big t.length),
	mstr(t.name),
	mstr("q"),
	mstr("q"),
	mstr("q"),
	};
	return put(0, l);
}

put(msg: int, l: list of array of byte): array of byte
{
	n := 4;
	if(!msg)
		n = 2;
	for(ll := l; ll != nil; ll = tl ll)
		n += len hd ll;
	d := array[n] of byte;
	for(l = rev(l); l != nil; l = tl l)
		d[n -= len hd l:] = hd l;
	if(msg)
		d[:] = m32(len d);
	else
		d[:] = m16(len d-2);
	return d;
}

Tabfile: adt {
	qp, mode, length, off:	int;
	name:			string;
};

Fids: adt {
	fids:	array of list of ref (int, int);

	new:	fn(): ref Fids;
	find:	fn(f: self ref Fids, i, fid: int): (int, int);
	del:	fn(f: self ref Fids, i, fid: int);
	add:	fn(f: self ref Fids, i, fid, isopen: int);
};

fidfind(l: list of ref (int, int), fid: int): ref (int, int)
{
	for(; l != nil; l = tl l)
		if((hd l).t0 == fid)
			return hd l;
	return nil;
}

fiddel(l: list of ref (int, int), fid: int): list of ref (int, int)
{
	r: list of ref (int, int);
	for(; l != nil; l = tl l)
		if((hd l).t0 != fid)
			r = hd l::r;
	return r;
}


Fids.new(): ref Fids
{
	return ref Fids(array[2] of list of ref (int, int));
}

Fids.find(f: self ref Fids, i, fid: int): (int, int)
{
	if(i < 0 || i == 0)
		r := fidfind(f.fids[0], fid);
	if(r != nil)
		return (0, r.t1);
	if(i < 0 || i == 1)
		r = fidfind(f.fids[1], fid);
	if(r != nil)
		return (1, r.t1);
	return (-1, 0);
}

Fids.del(f: self ref Fids, i, fid: int)
{
	f.fids[i] = fiddel(f.fids[i], fid);
}

Fids.add(f: self ref Fids, i, fid, isopen: int)
{
	f.fids[i] = ref(fid, isopen)::f.fids[i];
}

srv(f: ref Hdrfile, qp0, qp1: int, fd: ref Sys->FD)
{
	sys->pctl(Sys->NEWFD|Sys->NEWNS, list of {2, fd.fd});
	versionok := 0;
	fmode := 8r444;
	if(f.isdir)
		fmode = Sys->DMDIR|8r555;
	fids := Fids.new();
	tab := array[] of {
	Tabfile(qp0,	Sys->DMDIR|8r555,	0,		0,		"."),
	Tabfile(qp1,	fmode,			f.length,	f.offset,	f.name),
	};

	maxsize := 8*1024;
	for(;;) {
		sz := ref Buf(array[4] of byte, 0);
		rn := sys->readn(fd, sz.d, len sz.d);
		if(rn == 0)
			return say(sprint("tmsg eof"));
		if(rn != len sz.d)
			return say(sprint("read rmsg size: %r"));
		nsz := sz.g32();
		if(nsz > maxsize)
			return say(sprint("request too large, %d > %d", nsz, maxsize));
		b := ref Buf(array[nsz-4] of byte, 0);
		if(sys->readn(fd, b.d, len b.d) != len b.d)
			return say(sprint("read rmsg: %r"));

		t := b.g8();
		tag := b.g16();
		say(sprint("tmsg, t %d, tag %x", t, tag));
		if(t != Version && !versionok)
			return say("non-version tmsg without negotiated version");
		err: string;
		r: list of array of byte;
		case t {
		Version =>
			msize := b.g32();
			version := b.gstr();
			maxsize = 8*1024;
			if(msize < maxsize)
				maxsize = msize;
			n9p: con len "9P2000";
			proto := "9P2000";
			if(len version < n9p || version[:n9p] != "9P2000" || len version > n9p && version[n9p] != '.')
				proto = "unknown";
			versionok = (proto == "9P2000");
			fids = Fids.new();
			r = list of {m32(maxsize), mstr(proto)};
		Auth =>
			b.g32(); b.gstr(); b.gstr();
			err = "no auth";
		Attach =>
			fid := b.g32(); afid := b.g32(); b.gstr(); b.gstr();
			if(afid != Nofid)
				err = "afid not nofid";
			else if(fids.find(-1, fid).t0 >= 0)
				err = "fid in use";
			else {
				fids.add(0, fid, 0);
				r = list of {mqid(tab[0])};
			}
		Flush =>
			b.g16();
		Walk =>
			fid := b.g32(); nfid := b.g32(); nn := b.g16();
			if(nn > 16)
				return say("bad walk");
			names := array[nn] of string;
			for(j := 0; j < nn; j++)
				names[j] = b.gstr();
			(i, isopen) := fids.find(-1, fid);
say(sprint("walk, f.dir %q, f.name %q, fid %d, nfid %d, i %d, isopen %d", f.dir, f.name, fid, nfid, i, isopen));
			if(i < 0)
				err = "bad fid";
			else if(isopen)
				err = "fid open";
			else if(nfid != fid && fids.find(-1, nfid).t0 >= 0)
				err = "nfid in use";
			else if(nn > 0 && (tab[i].mode & Sys->DMDIR) == 0)
				err = "non-empty walk from non-dir";
			else {
				ni := i;
				rqids: list of array of byte;
				for(j = 0; j < nn; j++) {
					if(names[j] == "..")
						ni = 0;
					else if(ni == 0 && names[j] == tab[1].name)
						ni = 1;
					else
						break;
					rqids = mqid(tab[ni])::rqids;
				}
				if(nn > 0 && rqids == nil)
					err = sprint("%#q does not exist", names[0]);
				else {
					if(len rqids == nn) {
						if(fid == nfid)
							fids.del(i, fid);
						fids.add(ni, nfid, 0);
					}
					r = m16(len rqids)::rev(rqids);
				}
			}
		Open =>
			fid := b.g32(); mode := b.g8();
			(i, isopen) := fids.find(-1, fid);
			if(i < 0)
				err = "bad fid";
			else if(isopen)
				err = "already open";
			else if(mode != Sys->OREAD)
				err = "OREAD only";
			else {
				fids.del(i, fid);
				fids.add(i, fid, 1);
				r = list of {mqid(tab[i]), m32(maxsize-32)};
			}
		Create =>
			b.g32(); b.gstr(); b.g32(); b.g8();
			err = "permission denied";
		Read =>
			fid := b.g32(); off := b.g64(); count := b.g32();
			(i, isopen) := fids.find(-1, fid);
			if(i < 0)
				err = "bad fid";
			else if(!isopen)
				err = "fid not open";
			else {
				length := tab[i].length;
				bufoff := tab[i].off;
				buf := data;
				if(i == 0) {
					buf = mstat(tab[1]);
					length = len buf;
					bufoff = 0;
					if(off != big 0 && off != big len buf) {
						err = "bad directory seek";
						break;
					}
				}
				if(off > big length)
					off = big length;
				n := min(count, length-int off);
				o := bufoff+int off;
				say(sprint("read, fid %d, i %d, n %d, o %d", fid, i, n, o));
				r = list of {m32(n), buf[o:o+n]};
			}
		Write =>
			b.g32(); b.g64(); count := b.g32(); b.gn(count);
			err = "permission denied";
		Clunk or
		Remove =>
			fid := b.g32();
			(i, nil) := fids.find(-1, fid);
			if(i < 0) {
				err = "bad fid";
			} else {
				fids.del(i, fid);
				if(t == Remove)
					err = "permission denied";
			}
		Stat =>
			fid := b.g32();
			(i, nil) := fids.find(-1, fid);
			if(i < 0)
				err = "bad fid";
			else {
				d := mstat(tab[i]);
				r = list of {m16(len d), d};
			}
		Wstat =>
			b.g32(); b.gn(b.g16());
			err = "permission denied";
		}
		if(b.o != len b.d)
			return say("trailing data in rmsg");
		if(err != nil) {
			r = list of {m8(Error+1), m16(tag), mstr(err)};
			say(sprint("rmsg, error %q", err));
		} else {
			r = m8(t+1)::m16(tag)::r;
			say(sprint("rmsg, t %d+1", t));
		}
		rm := put(1, r);
		if(len rm > maxsize)
			return say("rmsg beyond max size");
		if(sys->write(fd, rm, len rm) != len rm)
			return say(sprint("write: %r"));
	}
}

rev[T](l: list of T): list of T
{
	r: list of T;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

l2a[T](l: list of T): array of T
{
	r := array[len l] of T;
	i := 0;
	for(; l != nil; l = tl l)
		r[i++] = hd l;
	return r;
}

min(a, b: int): int
{
	if(a < b)
		return a;
	return b;
}

warn(s: string)
{
	sys->fprint(sys->fildes(2), "%s\n", s);
}

say(s: string)
{
	if(dflag)
		warn(s);
}

fail(s: string)
{
	warn(s);
	raise "fail:"+s;
}
