module FHIR.XML.OneOf50 where

import Text.XML.HaXml.XmlContent

data OneOf50 a b c d e f g h i j k l m n o p q r s t u v w x y aa ab ac ad ae
             af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
    = OneOf50 a | TwoOf50 b | ThreeOf50 c | FourOf50 d | FiveOf50 e
    | SixOf50 f | SevenOf50 g | EightOf50 h | NineOf50 i | TenOf50 j
    | ElevenOf50 k | TwelveOf50 l | ThirteenOf50 m | FourteenOf50 n
    | FifteenOf50 o | SixteenOf50 p | SeventeenOf50 q | EighteenOf50 r
    | NineteenOf50 s | TwentyOf50 t | Choice21Of50 u | Choice22Of50 v
    | Choice23Of50 w | Choice24Of50 x | Choice25Of50 y | Choice26Of50 aa
    | Choice27Of50 ab | Choice28Of50 ac | Choice29Of50 ad | Choice30Of50 ae
    | Choice31Of50 af | Choice32Of50 ag | Choice33Of50 ah | Choice34Of50 ai
    | Choice35Of50 aj | Choice36Of50 ak | Choice37Of50 al | Choice38Of50 am
    | Choice39Of50 an | Choice40Of50 ao | Choice41Of50 ap | Choice42Of50 aq
    | Choice43Of50 ar | Choice44Of50 as | Choice45Of50 at | Choice46Of50 au
    | Choice47Of50 av | Choice48Of50 aw | Choice49Of50 ax | Choice50Of50 ay
    deriving (Eq,Show)

instance (HTypeable a,HTypeable b,HTypeable c,HTypeable d,HTypeable e
         ,HTypeable f,HTypeable g,HTypeable h,HTypeable i,HTypeable j
         ,HTypeable k,HTypeable l,HTypeable m,HTypeable n,HTypeable o
         ,HTypeable p,HTypeable q,HTypeable r,HTypeable s,HTypeable t
         ,HTypeable u,HTypeable v,HTypeable w,HTypeable x,HTypeable y
         ,HTypeable aa,HTypeable ab,HTypeable ac,HTypeable ad,HTypeable ae
         ,HTypeable af,HTypeable ag,HTypeable ah,HTypeable ai,HTypeable aj
         ,HTypeable ak,HTypeable al,HTypeable am,HTypeable an,HTypeable ao
         ,HTypeable ap,HTypeable aq,HTypeable ar,HTypeable as,HTypeable at
         ,HTypeable au,HTypeable av,HTypeable aw,HTypeable ax,HTypeable ay)
    => HTypeable (OneOf50 a b c d e f g h i j k l m n o p q r s t u v w x y
                           aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq
                           ar as at au av aw ax ay)
  where      toHType _ = Defined "OneOf50" [] []

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
         ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
         ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
         ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
         ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
         ,XmlContent aa,XmlContent ab,XmlContent ac,XmlContent ad
         ,XmlContent ae,XmlContent af,XmlContent ag,XmlContent ah
         ,XmlContent ai,XmlContent aj,XmlContent ak,XmlContent al
         ,XmlContent am,XmlContent an,XmlContent ao,XmlContent ap
         ,XmlContent aq,XmlContent ar,XmlContent as,XmlContent at
         ,XmlContent au,XmlContent av,XmlContent aw,XmlContent ax
         ,XmlContent ay)
    => XmlContent (OneOf50 a b c d e f g h i j k l m n o p q r s t u v w x y
                           aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq
                           ar as at au av aw ax ay)
  where
    parseContents =
        (choice OneOf50 $ choice TwoOf50 $ choice ThreeOf50 $ choice FourOf50
        $ choice FiveOf50 $ choice SixOf50 $ choice SevenOf50
        $ choice EightOf50 $ choice NineOf50 $ choice TenOf50
        $ choice ElevenOf50 $ choice TwelveOf50 $ choice ThirteenOf50
        $ choice FourteenOf50 $ choice FifteenOf50 $ choice SixteenOf50
        $ choice SeventeenOf50 $ choice EighteenOf50 $ choice NineteenOf50
        $ choice TwentyOf50 $ choice Choice21Of50 $ choice Choice22Of50
        $ choice Choice23Of50 $ choice Choice24Of50 $ choice Choice25Of50
        $ choice Choice26Of50 $ choice Choice27Of50 $ choice Choice28Of50
        $ choice Choice29Of50 $ choice Choice30Of50 $ choice Choice31Of50
        $ choice Choice32Of50 $ choice Choice33Of50 $ choice Choice34Of50
        $ choice Choice35Of50 $ choice Choice36Of50 $ choice Choice37Of50
        $ choice Choice38Of50 $ choice Choice39Of50 $ choice Choice40Of50
        $ choice Choice41Of50 $ choice Choice42Of50 $ choice Choice43Of50
        $ choice Choice44Of50 $ choice Choice45Of50 $ choice Choice46Of50
        $ choice Choice47Of50 $ choice Choice48Of50 $ choice Choice49Of50
        $ choice Choice50Of50
        $ fail "OneOf50")
    toContents (OneOf50 x) = toContents x
    toContents (TwoOf50 x) = toContents x
    toContents (ThreeOf50 x) = toContents x
    toContents (FourOf50 x) = toContents x
    toContents (FiveOf50 x) = toContents x
    toContents (SixOf50 x) = toContents x
    toContents (SevenOf50 x) = toContents x
    toContents (EightOf50 x) = toContents x
    toContents (NineOf50 x) = toContents x
    toContents (TenOf50 x) = toContents x
    toContents (ElevenOf50 x) = toContents x
    toContents (TwelveOf50 x) = toContents x
    toContents (ThirteenOf50 x) = toContents x
    toContents (FourteenOf50 x) = toContents x
    toContents (FifteenOf50 x) = toContents x
    toContents (SixteenOf50 x) = toContents x
    toContents (SeventeenOf50 x) = toContents x
    toContents (EighteenOf50 x) = toContents x
    toContents (NineteenOf50 x) = toContents x
    toContents (TwentyOf50 x) = toContents x
    toContents (Choice21Of50 x) = toContents x
    toContents (Choice22Of50 x) = toContents x
    toContents (Choice23Of50 x) = toContents x
    toContents (Choice24Of50 x) = toContents x
    toContents (Choice25Of50 x) = toContents x
    toContents (Choice26Of50 x) = toContents x
    toContents (Choice27Of50 x) = toContents x
    toContents (Choice28Of50 x) = toContents x
    toContents (Choice29Of50 x) = toContents x
    toContents (Choice30Of50 x) = toContents x
    toContents (Choice31Of50 x) = toContents x
    toContents (Choice32Of50 x) = toContents x
    toContents (Choice33Of50 x) = toContents x
    toContents (Choice34Of50 x) = toContents x
    toContents (Choice35Of50 x) = toContents x
    toContents (Choice36Of50 x) = toContents x
    toContents (Choice37Of50 x) = toContents x
    toContents (Choice38Of50 x) = toContents x
    toContents (Choice39Of50 x) = toContents x
    toContents (Choice40Of50 x) = toContents x
    toContents (Choice41Of50 x) = toContents x
    toContents (Choice42Of50 x) = toContents x
    toContents (Choice43Of50 x) = toContents x
    toContents (Choice44Of50 x) = toContents x
    toContents (Choice45Of50 x) = toContents x
    toContents (Choice46Of50 x) = toContents x
    toContents (Choice47Of50 x) = toContents x
    toContents (Choice48Of50 x) = toContents x
    toContents (Choice49Of50 x) = toContents x
    toContents (Choice50Of50 x) = toContents x

foldOneOf50 :: (a->z) -> (b->z) -> (c->z) -> (d->z) -> (e->z) -> (f->z) -> 
               (g->z) -> (h->z) -> (i->z) -> (j->z) -> (k->z) -> (l->z) -> 
               (m->z) -> (n->z) -> (o->z) -> (p->z) -> (q->z) -> (r->z) -> 
               (s->z) -> (t->z) -> (u->z) -> (v->z) -> (w->z) -> (x->z) -> 
               (y->z) -> (aa->z) -> (ab->z) -> (ac->z) -> (ad->z) -> 
               (ae->z) -> (af->z) -> (ag->z) -> (ah->z) -> (ai->z) -> 
               (aj->z) -> (ak->z) -> (al->z) -> (am->z) -> (an->z) -> 
               (ao->z) -> (ap->z) -> (aq->z) -> (ar->z) -> (as->z) -> 
               (at->z) -> (au->z) -> (av->z) -> (aw->z) -> (ax->z) -> 
               (ay->z) -> 
               OneOf50 a b c d e f g h i j k l m n o p q r s t u v w x y aa
                       ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as
                       at au av aw ax ay
               -> z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (OneOf50 z) = a z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (TwoOf50 z) = b z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (ThreeOf50 z) = c z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (FourOf50 z) = d z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (FiveOf50 z) = e z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (SixOf50 z) = f z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (SevenOf50 z) = g z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (EightOf50 z) = h z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (NineOf50 z) = i z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (TenOf50 z) = j z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (ElevenOf50 z) = k z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (TwelveOf50 z) = l z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (ThirteenOf50 z) = m z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (FourteenOf50 z) = n z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (FifteenOf50 z) = o z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (SixteenOf50 z) = p z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (SeventeenOf50 z) = q z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (EighteenOf50 z) = r z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (NineteenOf50 z) = s z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (TwentyOf50 z) = t z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice21Of50 z) = u z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice22Of50 z) = v z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice23Of50 z) = w z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice24Of50 z) = x z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice25Of50 z) = y z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice26Of50 z) = aa z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice27Of50 z) = ab z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice28Of50 z) = ac z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice29Of50 z) = ad z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice30Of50 z) = ae z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice31Of50 z) = af z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice32Of50 z) = ag z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice33Of50 z) = ah z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice34Of50 z) = ai z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice35Of50 z) = aj z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice36Of50 z) = ak z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice37Of50 z) = al z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice38Of50 z) = am z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice39Of50 z) = an z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice40Of50 z) = ao z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice41Of50 z) = ap z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice42Of50 z) = aq z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice43Of50 z) = ar z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice44Of50 z) = as z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice45Of50 z) = at z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice46Of50 z) = au z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice47Of50 z) = av z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice48Of50 z) = aw z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice49Of50 z) = ax z
foldOneOf50 a b c d e f g h i j k l m n o p q r s
            t u v w x y aa ab ac ad ae af ag ah
            ai aj ak al am an ao ap aq ar as at
            au av aw ax ay (Choice50Of50 z) = ay z

----
