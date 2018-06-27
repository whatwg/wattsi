# Wattsi Syntax

Like most specification preprocessors, Wattsi accepts as input a language that is based on HTML, but has extra features. We document those features, and how to use them, here.

## Cross references

Wattsi supports semi-automatic cross referencing to `<dfn>` elements defined elsewhere in the spec. Each `<dfn>` has a **canonical definition string** which is used to refer to it; this string must be unique among all `<dfn>` elements in the spec.

By default, the canonical definition string is the text content of the `<dfn>` element. It can be customized by using the `data-x=""` attribute on the definition. This is usually done when the text content is too generic to be unique.

When referencing a definition, you use the `<span>` or `<code>` elements. These work in a symmetrical manner: by default, they attempt to cross-reference a term according to their text content, but the term referenced can be overridden by using the `data-x=""` attribute.

### Examples

In this example, we use the default canonical definition string for the `<dfn>`. We later refer to it both by its full name, when that is appropriate, or by using `data-x=""` combined with a shortened text content, where the context is clear and we don't want to be long-winded.

```html
<p>A <dfn>custom element definition</dfn> consists of...</p>

<p>... These steps will return either a <span>custom element definition</span> or null.</p>

<p>... Eventually an appropriate <span data-x="custom element definition">definition</span> will be
registered.</p>
```

In this example, the `<dfn>`'s text content ("name") is too common to use the default behavior, so we assign it a custom canonical definition string by using `data-x=""`:

```html
<p>Each <span>custom element definition</span> has a <dfn
data-x="concept-custom-element-definition-name">name</dfn>.</p>

<p>In general, algorithms look up custom elements by any of <span
data-x="concept-custom-element-definition-name">name</span>, ...</p>
```

Note that the choice of `"concept-custom-element-definition-name"` was arbitrary; any unique string would suffice. However, since the ID for a `<dfn>` (and thus the URL fragment for any links to it) is derived from its canonical definition string, we try to choose ones that look nice.

Wattsi does not perform any automatic pluralization or stemming. Thus, if you want to refer to a concept by some variant of its canonical definition string, you'll need to use `data-x=""` on the `<span>` or `<code>`:

```html
<p>It may be preferable for <span data-x="custom element definition">custom element
definitions</span> to be...</p>
```

However, Wattsi is case-insensitive, so at least you don't have to worry about initial capitalization:

```html
<p><span>Custom element definitions</span> are awesome.</p>
```

If you need to use a `<span>` or `<code>` element without it being a cross-reference, then you can use an empty `data-x=""` attribute:

```html
<p>In the following document, the element definition for <code
data-x="">img-viewer</code> is loaded asynchronously:</p>
```

### Cross-specification cross references

Wattsi does not support any database of definitions across all specifications in the ecosystem (like Bikeshed does). To work around this, the HTML Standard has a <a href="https://html.spec.whatwg.org/multipage/infrastructure.html#dependencies">Dependencies</a> section where we create "proxy" definitions.

These proxy definitions use a new attribute, `data-x-href=""`, which indicates the external URL where the definition was originally located. For example:

```html
<p>The following terms are defined in the WHATWG Infra standard: <ref spec=INFRA></p>

<ul class="brief">
 <li>The general iteration terms <dfn data-x-href="https://infra.spec.whatwg.org/#iteration-while">while</dfn>,
         <dfn data-x-href="https://infra.spec.whatwg.org/#iteration-continue">continue</dfn>, and
         <dfn data-x-href="https://infra.spec.whatwg.org/#iteration-break">break</dfn>.</li>
 <li><dfn data-x-href="https://infra.spec.whatwg.org/#code-point">code point</dfn> and its synonym
     <dfn data-x-href="https://infra.spec.whatwg.org/#code-point">character</dfn></li>
 <li><dfn data-x-href="https://infra.spec.whatwg.org/#surrogate">surrogate</dfn></li>
 <li><dfn data-x-href="https://infra.spec.whatwg.org/#scalar-value">scalar value</dfn></li>
 <li><dfn data-x-href="https://infra.spec.whatwg.org/#noncharacter">noncharacter</dfn></li>
 ...
</ul>
```

Note how the above examples all use the default canonical definition text for the `<dfn>`. Sometimes we want to use a variant, as in the following example where the term is too common to be used by itself:

```html
<li>The <dfn data-x-href="https://infra.spec.whatwg.org/#list">list</dfn> data structure and the associated definitions for
        <dfn data-x="list append" data-x-href="https://infra.spec.whatwg.org/#list-append">append</dfn>,
        <dfn data-x="list replace" data-x-href="https://infra.spec.whatwg.org/#list-remove">replace</dfn>,
        ...</li>
```

We can then use these "proxy definitions" as normal:

```html
<p>Each <code>DOMStringList</code> object has an associated <span>list</span>.</p>

<p><span data-x="list append">Append</span> <var>serializedEntry</var> to
<var>serialized</var>.[[SetData]].</p>
```

### Exporting definitions

Definitions that are meant to be used by other specifications need to be exported using the [Bikeshed definition data model](https://tabatkins.github.io/bikeshed/#dfn-contract). In practice, this means adding `data-export=""` to the `<dfn>`, and if appropriate, `data-dfn-type=""`, `data-lt=""`, or `data-dfn-for=""` attributes. Some examples:

```html
<p>The <code>Document</code> has an <dfn data-x="concept-document-https-state" data-export=""
data-dfn-for="Document">HTTPS state</dfn>...</p>
```

```html
<dt><dfn data-x="external resource link" data-lt="external resource link" data-export="">Links to
external resources</dfn></dt>
```

```html
<dt><dfn data-export="" data-dfn-type="selector"><code
data-x="selector-defined">:defined</code></dfn></dt>
```

## References to other specifications

To produce the familiar bracketed references to other specifications, e.g.

> There are a number of dynamic selectors that can be used with HTML. This section defines when these selectors match HTML elements. [[SELECTORS]](https://html.spec.whatwg.org/#refsSELECTORS) [[CSSUI]](https://html.spec.whatwg.org/#refsCSSUI)

you can use the `<ref>` void element:

```html
<p>There are a number of dynamic selectors that can be used with HTML. This section defines when
these selectors match HTML elements. <ref spec=SELECTORS> <ref spec=CSSUI></p>
```

These match against a bibliography, which is a manually-maintained and sorted `<dl>` at the end of the source file. Its entries look like the following:

```html
<dt id="refsSELECTORS">[SELECTORS]</dt>
<dd><cite><a href="https://drafts.csswg.org/selectors/">Selectors</a></cite>, E. Etemad, T. &Ccedil;elik, D. Glazman, I. Hickson, P. Linss, J. Williams. W3C.</dd>
```

## Specification variants

There are currently up to five variants of the specification produced on each build:

- [Singlepage](https://html.spec.whatwg.org/)
- [Multipage](https://html.spec.whatwg.org/multipage/)
- [Developer's edition](https://html.spec.whatwg.org/dev/)
- Commit snapshots
- Review drafts

To selectively include or omit content from these variants, you can use the following attributes on any element:

|Edition            |Include |Omit      |
|-------------------|--------|----------|
|Singlepage         |w-html  |w-nohtml  |
|Multipage          |w-split |w-nosplit |
|Developer's edition|w-dev   |w-nodev   |
|Commit snapshots   |w-snap  |w-nosnap  |
|Review drafts      |w-review|w-noreview|

The most important of these for day-to-day work is **w-nodev**, which excludes content (such as details only interesting to web browser implementers) from the developer's edition.

## Macros

Wattsi has a couple of macros used to insert common text fragments:

* `<!-- NON-NORMATIVE SECTION -->` inserts a "This section is non-normative." notice. ([Example](https://html.spec.whatwg.org/multipage/introduction.html#suggested-reading))
* `<!--INSERT FINGERPRINT-->` inserts a fingerprint image to indicate a feature that may cause fingerprinting. (See the "[Privacy concerns](https://html.spec.whatwg.org/multipage/introduction.html#fingerprint)" section)
