import argparse
from typing import Optional
from pathlib import Path as PLPath
import xml.etree.ElementTree as ET
from dataclasses import dataclass
import json
import io
from datetime import date

from svgelements import Length, Rect, Viewbox, Matrix, Path, Point, Ellipse, \
    Polygon, Group
import cairosvg
from PIL import Image
import numpy as np
from skimage import measure
from tqdm import tqdm


def make_args():
    parser = argparse.ArgumentParser(description='Process some integers.')
    parser.add_argument('-t', '--type', type=str, default="prolog", choices=['prolog', 'coco'],
                        help='output type')
    parser.add_argument('-o', '--output', type=PLPath, help='output file')
    parser.add_argument('--categories', type=PLPath, help='existing categories', default=None)
    parser.add_argument('glyphnames', type=PLPath, help='glyphnames.json file')
    parser.add_argument('svg', type=PLPath, nargs='+', help='svg files to parse')

    args = parser.parse_args()
    return args


def pointtopl(point: Optional[Point]):
    if point:
        return f"point({point.x}, {point.y})"
    else:
        return "_"


class _List(list):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.sep = ' '
        self.start = ''
        self.end = ''

    def topl(self):
        return f"[{self.start}" + f",{self.sep}".join(
                [x.topl() for x in self]) + f"{self.end}]"


@dataclass
class IdClass:
    id: Optional[str]
    c: str

    def topl(self):
        if self.id:
            if isinstance(self.id, int):
                id = f"{self.id}"
            else:
                id = f"'{self.id}'"
        else:
            id = '_'
        if self.c is None:
            c = '_'
        else:
            c = f"'{self.c}'"
        return f"{id}-{c}"


def composite_elements_to_image(elements, page_width, page_height):
    background = Image.new(mode='RGBA', size=(page_width, page_height),
                           color='white')
    for element in elements:
        x, y, width, height = element.bbox()
        img = element.toimage(fill='black')
        if img is not None:
            img = img.convert('RGBA')
            background.alpha_composite(img, (int(x), int(y)))
    return background


def dtorle(el, parent_width, parent_height):
    x, y = el.lefttop.x, el.lefttop.y
    arr = np.array(el.toimage(fill='white').convert('L'))
    
    # Find contours directly on the small crop
    labelMask = arr == 255
    contours = measure.find_contours(labelMask, 0.5)
    
    # Calculate the exact area of the mask (number of pixels)
    actual_area = int(np.sum(labelMask))
    
    polygons = []
    for contour in contours:
        # flip (row, col) to (x, y)
        contour = np.flip(contour, axis=1)
        # Add the exact float bounding box offset for maximum precision
        contour[:, 0] += x
        contour[:, 1] += y
        polygons.append(contour.ravel().tolist())
        
    return actual_area, polygons


@dataclass
class Seg:
    d: str | None
    start: Point
    end: Point
    etiq: list
    thickness: float

    def topl(self):
        return f"seg({pointtopl(self.start)}, {pointtopl(self.end)}, " \
                f"{_List(self.etiq[::-1]).topl()}, {self.thickness})"

    def bbox(self):
        delta = self.thickness / 2
        x = min(self.start.x, self.end.x) - delta
        xmax = max(self.start.x, self.end.x) + delta
        width = xmax - x
        y = min(self.start.y, self.end.y) - delta
        ymax = max(self.start.y, self.end.y) + delta
        height = ymax - y
        return (x, y, width, height)

    def toimage(self, fill='white'):
        g = Group()
        x, y, width, height = self.bbox()
        if width < 1 or height < 1:
            return None
        if self.etiq[-2].c in ['beam', 'beamSpan']:
            opts = {'fill': fill}
        else:
            opts = {'stroke': fill, 'stroke_width': self.thickness}
        g.append(Path(d=self.d, **opts,
                      transform=Matrix.translate(-x, -y)).reify())
        xml = g.string_xml()
        img = cairosvg.svg2png(
            xml, parent_width=width,
            parent_height=height)
        img = Image.open(io.BytesIO(img))
        return img

    def tococo(self, i, page_id, parent_width, parent_height):
        bbox = self.bbox()
        left, top, width, height = bbox
        if width < 1 or height < 1:
            return None
        right = left + width
        bottom = top + height
        polygon = [left, top, left, bottom, right, bottom, right, top]
        category = self.etiq[-2].c
        category = 'beam' if category == 'beamSpan' else category
        area = width * height
        return {
            'id': i,
            'image_id': page_id,
            'category': category,
            'supercategory': self.etiq[-1].c,
            'segmentation': [polygon],
            'area': area,
            'bbox': list(bbox),
            'iscrowd': 0,
            'keypoints': [self.start.x, self.start.y, 2,
                          self.end.x, self.end.y, 2],
            'num_keypoints': 2,
        }


@dataclass
class Ccx:
    d: str
    lefttop: Point
    rightbottom: Point
    etiq: list[tuple[Optional[str], str]]
    origin: Point = None

    def topl(self):
        return f"ccx({pointtopl(self.lefttop)}, " \
               f"{pointtopl(self.rightbottom)}, " \
               f"{_List(self.etiq[::-1]).topl()}, " \
               f"{pointtopl(self.origin)})"

    def bbox(self):
        x, y = self.lefttop.x, self.lefttop.y
        xmax, ymax = self.rightbottom.x, self.rightbottom.y
        width, height = xmax - x, ymax - y
        return x, y, width, height

    def toimage(self, fill='white'):
        g = Group()
        x, y, width, height = self.bbox()
        if width == 0 or height == 0:
            return None
        g.append(Path(d=self.d, fill=fill,
                      transform=Matrix.translate(-x, -y)).reify())
        xml = g.string_xml()
        img = cairosvg.svg2png(
            xml, parent_width=width,
            parent_height=height)
        img = Image.open(io.BytesIO(img))
        return img

    def tococo(self, i, page_id, parent_width, parent_height):
        bbox = self.bbox()
        _, _, width, height = bbox
        if width < 1 or height < 1:
            return None
        area, polygons = dtorle(self, parent_width, parent_height)
        return {
            'id': i,
            'image_id': page_id,
            'category': self.etiq[-1].c,
            'supercategory': self.etiq[-3].c,
            'segmentation': polygons,
            'area': area,
            'bbox': list(bbox),
            'iscrowd': 0,
            'keypoints': [float(self.origin.x), float(self.origin.y), 2],
            'num_keypoints': 1,
        }


ns = {'svg': "{http://www.w3.org/2000/svg}",
      'xlink': "{http://www.w3.org/1999/xlink}"}


def get_tag(node):
    ntag = node.tag
    for k, v in ns.items():
        if v in ntag:
            ntag = ntag.replace(v, k + ':')
    return ntag


def get_attrib(node, attrib):
    name, attrib = attrib.split(':')
    link = ns[name]
    attrib = link + attrib
    return node.attrib[attrib]


def get_viewport(transforms):
    for transform in transforms:
        if isinstance(transform, Rect):
            return transform


def apply_transforms(obj, transforms):
    inv_trans = transforms[::-1]
    for i, transform in enumerate(inv_trans):
        if isinstance(transform, Matrix):
            obj = obj * transform
        elif isinstance(transform, Viewbox):
            viewport = get_viewport(inv_trans[i + 1:])
            obj = obj * transform.transform(viewport)
    return obj


def recurse(f):
    def wrapper(node, *args):
        node_res = f(node, *args)
        res = []
        if node_res is not None:
            res.append(node_res)
        for child in node:
            child_res = parse_node(child, *args)
            if isinstance(child_res, list):
                res.extend(child_res)
            elif child_res is not None:
                res.append(child_res)
        return res
    return wrapper


def backtrack(f):
    def wrapper(node, *args):
        args_pre = []
        for arg in args:
            if isinstance(arg, list):
                args_pre.append(len(arg))
            elif isinstance(arg, dict):
                args_pre.append(set(arg.keys()))
        res = f(node, *args)
        for arg, arg_pre in zip(args, args_pre):
            if isinstance(arg, list):
                assert len(arg) >= arg_pre
                for _ in range(len(arg) - arg_pre):
                    arg.pop(-1)
            elif isinstance(arg, dict):
                assert arg_pre.issubset(set(arg.keys()))
                for k in set(arg.keys()) - arg_pre:
                    del arg[k]
        return res
    return wrapper


@backtrack
@recurse
def parse_svgnode(node, transforms, defs, scopes):
    res = None
    if w := node.attrib.get('width'):
        if h := node.attrib.get('height'):
            # scopes.append(IdClass(None, 'page'))
            width = Length(w)
            height = Length(h)
            transforms.append(Rect(0, 0, width, height))
            res = Ccx(None, Point(0, 0), Point(width.value(), height.value()),
                      scopes.copy(), Point(0, 0))
    if viewBox := node.attrib.get('viewBox'):
        transforms.append(Viewbox(viewBox))
    return res


def parse_defs(node, transforms, defs, scopes):
    for child in node:
        defs[child.attrib['id']] = child
    return


@backtrack
@recurse
def parse_g(node, transforms, defs, scopes):
    if transform := node.attrib.get('transform'):
        transforms.append(Matrix(transform))
    if gclass := node.attrib.get('class'):
        if "ledgerLines" in gclass or 'octave' in gclass or 'slur' in gclass \
                or 'tie' in gclass:
            gclass = gclass.split(' ')[0]
        node_id = node.attrib.get('id', None)
        for scope in scopes:
            if gclass == scope.c and node_id is None:
                break
        else:
            scopes.append(IdClass(node_id, gclass))


def seg_swap(direction, start, end):
    if direction == 'h' and start.x > end.x:
        start, end = end, start
    if direction == 'v' and start.y > end.y:
        start, end = end, start
    return start, end


def poly_swap(direction, points):
    if direction == 'h':
        return sorted(points, key=lambda p: (p.x, p.y))
    if direction == 'v':
        return sorted(points, key=lambda p: (p.y, p.x))


@backtrack
def parse_path(node, transforms, defs, scopes):
    p = Path(**node.attrib)
    p = apply_transforms(p, transforms)
    p.reify()
    h_lines = ['staff', 'ledgerLines', 'octave']
    v_lines = ['barLine', 'system', 'stem']
    hv_lines = ['voltaBracket']
    if scopes and scopes[-1].c in h_lines + v_lines + hv_lines:
        points = [point for point in p.as_points()]
        if scopes[-1].c in h_lines:
            start, end = seg_swap('h', points[0], points[-1])
        elif scopes[-1].c in v_lines:
            start, end = seg_swap('v', points[0], points[-1])
        elif scopes[-1].c in hv_lines:
            if points[-1].x - points[0].x > points[-1].y - points[0].y:
                start, end = seg_swap('h', points[0], points[-1])
            else:
                start, end = seg_swap('v', points[0], points[-1])
        seg_scopes = scopes.copy()
        seg_scopes.append(IdClass(p.id, 'seg'))
        return Seg(p.d(), start, end, seg_scopes, p.stroke_width)
    elif scopes and scopes[-1].c == 'beamSpan':
        p1, p2, width = poly_to_hseg(p.as_points())
        seg_scopes = scopes.copy()
        seg_scopes.append(IdClass(p.id, 'seg'))
        return Seg(p.d(), p1, p2, seg_scopes, width)
    else:
        left, top, right, bottom = p.bbox()
        origin = Point(float(node.attrib.get('x', left)),
                       float(node.attrib.get('y', top)))
        ccx_scopes = scopes.copy()
        if label := node.attrib.get('class', None):
            ccx_scopes.append(IdClass(p.id, label))
        return Ccx(p.d(), Point(left, top), Point(right, bottom), ccx_scopes,
                   origin)


@backtrack
def parse_use(node, transforms, defs, scopes):
    attrib = node.attrib
    x = Length(attrib.get('x', 0))
    y = Length(attrib.get('y', 0))
    if 'transform' in attrib:
        transforms.append(Matrix(attrib['transform']))
    origin = apply_transforms(Point(x, y), transforms)
    transforms.append(Matrix.translate(x, y))
    if w := attrib.get('width'):
        if h := attrib.get('height'):
            transforms.append(Rect(0, 0, Length(w), Length(h)))
    href = get_attrib(node, 'xlink:href')
    symbol = defs[href.replace('#', '')]
    [ccx] = parse_node(symbol, transforms, defs, scopes)
    ccx.origin = origin
    glyphcode = href.split('-')[0]
    ccx.etiq.append(IdClass(node.attrib.get('id', None), glyphcode))
    return [ccx]


@backtrack
@recurse
def parse_symbol(node, transforms, defs, scopes):
    scopes.append(IdClass(None, 'symbol'))
    if viewBox := node.attrib.get('viewBox'):
        transforms.append(Viewbox(viewBox))


def parse_rect(node, transforms, defs, scopes):
    r = Rect(**node.attrib)
    r = apply_transforms(r, transforms)
    r.reify()
    if scopes[-1].c in ['stem', 'grpSym']:
        x, y, w, h = r.x, r.y, r.width, r.height
        x = x + (w / 2)
        seg_scopes = scopes.copy()
        seg_scopes.append(IdClass(node.attrib.get('id', None), 'seg'))
        return Seg(r.d(), Point(x, y), Point(x, y + h), seg_scopes, w)


def parse_ellipse(node, transforms, defs, scopes):
    e = Ellipse(**node.attrib)
    e = apply_transforms(e, transforms)
    e.reify()
    if scopes[-1].c == 'dots':
        left, top, right, bottom = e.bbox()
        return Ccx(e.d(), Point(left, top), Point(right, bottom),
                   scopes.copy(), Point(e.cx, e.cy))


def parse_polygon(node, transforms, defs, scopes):
    r = Polygon(**node.attrib)
    r = apply_transforms(r, transforms)
    r.reify()
    if scopes[-1].c in ['beam', 'beamSpan']:
        if len(set(r.points)) < 4:
            return None
        p1, p2, width = poly_to_hseg(r.points)
        seg_scopes = scopes.copy()
        seg_scopes.append(IdClass(node.attrib.get('id', None), 'seg'))
        return Seg(r.d(), p1, p2, seg_scopes, width)


def poly_to_hseg(points):
    lefttop, leftbottom, righttop, rightbottom = sorted(
        set(points), key=lambda p: (p.x, p.y))
    p1 = Point((lefttop.x + leftbottom.x) / 2,
               (lefttop.y + leftbottom.y) / 2)
    p2 = Point((righttop.x + rightbottom.x) / 2,
               (righttop.y + rightbottom.y) / 2)
    width = abs(leftbottom.y - lefttop.y)
    return p1, p2, width


# def parse_text(node, transforms, defs, scopes):
#     __import__('ipdb').set_trace()
#     return


def parse_node(node, transforms, defs, scopes):
    tag = get_tag(node)
    res = None
    if tag == "svg:svg":
        res = parse_svgnode(node, transforms, defs, scopes)
    elif tag == "svg:defs":
        res = parse_defs(node, transforms, defs, scopes)
    elif tag == "svg:g" and node.attrib.get('class') != 'pgFoot autogenerated':
        res = parse_g(node, transforms, defs, scopes)
    elif tag == "svg:path":
        res = parse_path(node, transforms, defs, scopes)
    elif tag == "svg:use":
        res = parse_use(node, transforms, defs, scopes)
    elif tag == "svg:symbol":
        res = parse_symbol(node, transforms, defs, scopes)
    elif tag == "svg:rect":
        res = parse_rect(node, transforms, defs, scopes)
    elif tag == "svg:ellipse":
        res = parse_ellipse(node, transforms, defs, scopes)
    elif tag == "svg:polygon":
        res = parse_polygon(node, transforms, defs, scopes)
    # elif tag == "svg:text":
    #     res = parse_text(node, transforms, defs, scopes)
    return res


def sort_elements(element):
    if isinstance(element, Seg):
        p = element.start
    elif isinstance(element, Ccx):
        p = element.origin
    x = p.x.value() if isinstance(p.x, Length) else p.x
    y = p.y.value() if isinstance(p.y, Length) else p.y
    return (x, y)


def load_glyphnames(path: Path) -> dict[str, str]:
    with path.open() as f:
        glyphnames = json.load(f)
    glyphnames_inv = {
        v['codepoint']: {'name': k, 'description': v['description']}
        for k, v in glyphnames.items()
    }
    return glyphnames_inv


def parse_svg(svg: PLPath, glyphnames: PLPath):
    print(svg)
    try:
        page_number = int(svg.stem.split('_')[-1])
    except ValueError:
        page_number = 1
    tree = ET.parse(svg)
    root = tree.getroot()
    res = parse_node(root, [], {}, [IdClass(page_number, 'page')])
    glyphnames_inv = load_glyphnames(glyphnames)
    for el in res:
        for i in range(len(el.etiq)):
            if label := el.etiq[i].c:
                if label.startswith('#'):
                    codepoint = el.etiq[i].c.replace('#', 'U+')
                    el.etiq[i] = IdClass(el.etiq[i].id,
                                         glyphnames_inv[codepoint]['name'])
    res.sort(key=sort_elements)
    res = _List(res)
    res.sep = '\n\t'
    res.start = '\n\t'
    res.end = '\n'
    return res


def make_categories(annotations, categories):
    if categories:
        with categories.open() as f:
            coco_categories = json.load(f)['categories']
        category_map = {cat['name']: cat['id'] for cat in coco_categories}
    else:
        categories = set()
        names = set()
        for ann in annotations:
            category = ann['category']
            supercategory = ann['supercategory']
            if ann['num_keypoints'] == 1:
                keypoints = ['origin']
            elif ann['num_keypoints'] == 2:
                keypoints = ['start', 'end']
            else:
                raise RuntimeError("Unknown number of keypoints")
            if category not in names:
                categories.add((category, supercategory, tuple(keypoints)))
                names.add(category)
        categories = sorted(categories)
        coco_categories = []
        category_map = {}
        for i, (category, supercategory, keypoints) in enumerate(categories, start=1):
            coco_categories.append({
                'id': i,
                'name': category,
                'supercategory': supercategory,
                'keypoints': list(keypoints),
            })
            category_map[category] = i
    for i, ann in enumerate(annotations, start=1):
        ann['category_id'] = category_map[ann['category']]
        ann['id'] = i

    return annotations, coco_categories


def main(args):
    page_elements = _List([parse_svg(svg, args.glyphnames) for svg in tqdm(args.svg)])
    page_elements.sep = '\n\t'
    page_elements.start = '\n\t'
    page_elements.end = '\n'
    if args.type == 'prolog':
        with args.output.open('w') as f:
            page_elements = _List([el for page in page_elements for el in page])
            f.write(page_elements.topl() + '.')
    elif args.type == 'coco':
        annotations = []
        images = []
        for page_id, res in enumerate(tqdm(page_elements), start=1):
            page = res[0]
            width = int(page.rightbottom.x)
            height = int(page.rightbottom.y)
            svg = args.svg[page_id-1]
            png = svg.with_suffix('.png')
            image = composite_elements_to_image(res[1:], width, height)
            image.convert('L').save(png)
            for i, el in enumerate(tqdm(res[1:]), start=1):
                annotation = el.tococo(i, page_id, width, height)
                if annotation is not None:
                    annotations.append(annotation)
            images.append({
                'id': page_id,
                'width': width,
                'height': height,
                'file_name': str(png.name),
            })
        annotations, categories = make_categories(annotations, args.categories)
        data = {
            'info': {
                'year': date.today().year,
                'version': '1.0',
                'description': 'Music Symbol Coco Dataset',
                'contributor': 'Kwon-Young Choi',
                'url': 'https://github.com/kwon-young/music',
                'date_created': '2024-02-10',
            },
            'images': images,
            'annotations': annotations,
            'categories': categories,
        }
        with args.output.open('w') as f:
            json.dump(data, f)
    return


if __name__ == "__main__":
    main(make_args())
